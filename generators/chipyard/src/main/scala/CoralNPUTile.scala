package chipyard

import chisel3._

import org.chipsalliance.cde.config.{Config, Parameters}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.resources._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class CoralNPUCoreParams(
  bootFreqHz: BigInt = BigInt(1700000000),
  enableFloat: Boolean = false,
  enableRvv: Boolean = false
) extends CoreParams {
  val xLen: Int = 32
  val pgLevels: Int = 2
  val useVM: Boolean = false
  val useHypervisor: Boolean = false
  val useUser: Boolean = true
  val useSupervisor: Boolean = false
  val useDebug: Boolean = true
  val useAtomics: Boolean = true
  val useAtomicsOnlyForIO: Boolean = false
  val useCompressed: Boolean = false
  override val useVector: Boolean = enableRvv
  val useSCIE: Boolean = false
  val useRVE: Boolean = false
  val mulDiv: Option[MulDivParams] = Some(MulDivParams())
  val fpu: Option[FPUParams] = if (enableFloat) Some(FPUParams()) else None
  val nLocalInterrupts: Int = 0
  val useNMI: Boolean = false
  val nPTECacheEntries: Int = 0
  val nPMPs: Int = 0
  val pmpGranularity: Int = 4
  val nBreakpoints: Int = 0
  val useBPWatch: Boolean = false
  val mcontextWidth: Int = 0
  val scontextWidth: Int = 0
  val nPerfCounters: Int = 0
  val haveBasicCounters: Boolean = true
  val haveFSDirty: Boolean = false
  val misaWritable: Boolean = false
  val haveCFlush: Boolean = false
  val nL2TLBEntries: Int = 0
  val nL2TLBWays: Int = 1
  val mtvecInit: Option[BigInt] = Some(BigInt(0))
  val mtvecWritable: Boolean = true
  val instBits: Int = 32
  val lrscCycles: Int = 80
  val decodeWidth: Int = 1
  val fetchWidth: Int = 1
  val retireWidth: Int = 1
  val traceHasWdata: Boolean = false
  val useConditionalZero: Boolean = false
  val useZba: Boolean = false
  val useZbb: Boolean = true
  val useZbs: Boolean = false
}

case class CoralNPUTileAttachParams(
  tileParams: CoralNPUTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = CoralNPUTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}

case class CoralNPUTileParams(
  name: Option[String] = Some("coralnpu_tile"),
  tileId: Int = 0,
  trace: Boolean = false,
  coralBaseAddress: BigInt = BigInt("70000000", 16),
  coralWindowBytes: BigInt = BigInt("400000", 16),
  itcmSizeKBytes: Int = 8,
  dtcmSizeKBytes: Int = 32,
  testEnable: Boolean = true,
  axiAddrBits: Int = 32,
  axiDataBits: Int = 256,
  axiIdBits: Int = 6,
  val core: CoralNPUCoreParams = CoralNPUCoreParams()
) extends InstantiableTileParams[CoralNPUTile] {
  require(axiDataBits % 8 == 0, "CoralNPU AXI data width must be byte aligned.")
  require(coralWindowBytes > 0, "CoralNPU MMIO window must be non-zero.")

  val baseName = name.getOrElse("coralnpu_tile")
  val uniqueName = s"${baseName}_$tileId"
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val btb: Option[BTBParams] = None
  val boundaryBuffers: Boolean = false
  val dcache: Option[DCacheParams] = None
  val icache: Option[ICacheParams] = None
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()

  val axiDataBytes: Int = axiDataBits / 8

  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): CoralNPUTile = {
    new CoralNPUTile(this, crossing, lookup)
  }
}

class CoralNPUTile private(
  val coralnpuParams: CoralNPUTileParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters)
  extends BaseTile(coralnpuParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications {

  def this(params: CoralNPUTileParams, crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = None
  val masterNode = visibilityNode
  val slaveNode = TLIdentityNode()

  tlOtherMastersNode := tlMasterXbar.node
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("google,coralnpu", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty ++ tileProperties)
    }
  }

  val coralDevice = new SimpleDevice("coralnpu", Seq("google,coralnpu-axi"))

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
    Resource(coralDevice, "reg").bind(ResourceAddress(coralnpuParams.coralBaseAddress))
  }

  val coralMasterNode = AXI4MasterNode(
    Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = s"coralnpu-master-$tileId",
        id = IdRange(0, 1 << coralnpuParams.axiIdBits))))))

  (tlMasterXbar.node
    := TLBuffer()
    := TLFIFOFixer(TLFIFOFixer.all)
    := TLWidthWidget(masterPortBeatBytes)
    := AXI4ToTL()
    := AXI4UserYanker(capMaxFlight = Some(1 << coralnpuParams.axiIdBits))
    := AXI4Fragmenter()
    := coralMasterNode)

  val coralSlaveNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = AddressSet.misaligned(coralnpuParams.coralBaseAddress, coralnpuParams.coralWindowBytes),
      resources = coralDevice.reg,
      regionType = RegionType.UNCACHED,
      executable = true,
      supportsRead = TransferSizes(1, coralnpuParams.axiDataBytes),
      supportsWrite = TransferSizes(1, coralnpuParams.axiDataBytes),
      interleavedId = Some(0))),
    beatBytes = coralnpuParams.axiDataBytes)))

  (coralSlaveNode
    := AXI4Buffer()
    := AXI4UserYanker()
    := AXI4Deinterleaver(coralnpuParams.axiDataBytes)
    := AXI4IdIndexer(coralnpuParams.axiIdBits)
    := TLToAXI4()
    := TLWidthWidget(coralnpuParams.axiDataBytes)
    := slaveNode)

  override lazy val module = new CoralNPUTileModuleImp(this)
}

class CoralNPUTileModuleImp(outer: CoralNPUTile) extends BaseTileModuleImp(outer) {
  Annotated.params(this, outer.coralnpuParams)

  private val coralRegions = coralnpu.MemoryRegions.highmem(
    outer.coralnpuParams.itcmSizeKBytes,
    outer.coralnpuParams.dtcmSizeKBytes
  ).map { r =>
    val rebasedStart = BigInt(r.memStart) + outer.coralnpuParams.coralBaseAddress
    require(rebasedStart >= 0 && rebasedStart <= Int.MaxValue, "CoralNPU memory regions must fit in 31-bit signed addresses.")
    new coralnpu.MemoryRegion(rebasedStart.toInt, r.memSize, r.memType)
  }

  private val coralParams = new coralnpu.Parameters(coralRegions, outer.tileId)
  coralParams.enableFloat = outer.coralnpuParams.core.enableFloat
  coralParams.enableRvv = outer.coralnpuParams.core.enableRvv
  coralParams.itcmSizeKBytes = outer.coralnpuParams.itcmSizeKBytes
  coralParams.dtcmSizeKBytes = outer.coralnpuParams.dtcmSizeKBytes

  val core = Module(new coralnpu.CoreAxi(coralParams, s"CoralNPU_${outer.tileId}"))

  core.io.aclk := clock
  core.io.aresetn := (~reset.asBool).asAsyncReset
  core.io.te := outer.coralnpuParams.testEnable.B
  core.io.boot_addr := outer.resetVectorSinkNode.bundle(31, 0)

  val intBundle = Wire(new TileInterrupts)
  outer.decodeCoreInterrupts(intBundle)
  core.io.irq := intBundle.debug || intBundle.msip || intBundle.mtip || intBundle.meip || intBundle.seip.getOrElse(false.B)

  core.io.dm.req.valid := false.B
  core.io.dm.req.bits := 0.U.asTypeOf(core.io.dm.req.bits)
  core.io.dm.rsp.ready := true.B

  outer.reportWFI(Some(core.io.wfi))
  outer.reportHalt(Some(core.io.fault))
  outer.reportCease(Some(core.io.halted))

  outer.coralMasterNode.out.foreach { case (out, _) =>
    out.aw.bits := 0.U.asTypeOf(out.aw.bits)
    out.aw.valid := core.io.axi_master.write.addr.valid
    out.aw.bits.id := core.io.axi_master.write.addr.bits.id
    out.aw.bits.addr := core.io.axi_master.write.addr.bits.addr
    out.aw.bits.len := core.io.axi_master.write.addr.bits.len
    out.aw.bits.size := core.io.axi_master.write.addr.bits.size
    out.aw.bits.burst := core.io.axi_master.write.addr.bits.burst
    out.aw.bits.lock := core.io.axi_master.write.addr.bits.lock
    out.aw.bits.cache := core.io.axi_master.write.addr.bits.cache
    out.aw.bits.prot := core.io.axi_master.write.addr.bits.prot
    out.aw.bits.qos := core.io.axi_master.write.addr.bits.qos
    out.aw.bits.region := core.io.axi_master.write.addr.bits.region
    core.io.axi_master.write.addr.ready := out.aw.ready

    out.w.bits := 0.U.asTypeOf(out.w.bits)
    out.w.valid := core.io.axi_master.write.data.valid
    out.w.bits.data := core.io.axi_master.write.data.bits.data
    out.w.bits.strb := core.io.axi_master.write.data.bits.strb
    out.w.bits.last := core.io.axi_master.write.data.bits.last
    core.io.axi_master.write.data.ready := out.w.ready

    core.io.axi_master.write.resp.valid := out.b.valid
    core.io.axi_master.write.resp.bits.id := out.b.bits.id
    core.io.axi_master.write.resp.bits.resp := out.b.bits.resp
    out.b.ready := core.io.axi_master.write.resp.ready

    out.ar.bits := 0.U.asTypeOf(out.ar.bits)
    out.ar.valid := core.io.axi_master.read.addr.valid
    out.ar.bits.id := core.io.axi_master.read.addr.bits.id
    out.ar.bits.addr := core.io.axi_master.read.addr.bits.addr
    out.ar.bits.len := core.io.axi_master.read.addr.bits.len
    out.ar.bits.size := core.io.axi_master.read.addr.bits.size
    out.ar.bits.burst := core.io.axi_master.read.addr.bits.burst
    out.ar.bits.lock := core.io.axi_master.read.addr.bits.lock
    out.ar.bits.cache := core.io.axi_master.read.addr.bits.cache
    out.ar.bits.prot := core.io.axi_master.read.addr.bits.prot
    out.ar.bits.qos := core.io.axi_master.read.addr.bits.qos
    out.ar.bits.region := core.io.axi_master.read.addr.bits.region
    core.io.axi_master.read.addr.ready := out.ar.ready

    core.io.axi_master.read.data.valid := out.r.valid
    core.io.axi_master.read.data.bits.id := out.r.bits.id
    core.io.axi_master.read.data.bits.data := out.r.bits.data
    core.io.axi_master.read.data.bits.resp := out.r.bits.resp
    core.io.axi_master.read.data.bits.last := out.r.bits.last
    out.r.ready := core.io.axi_master.read.data.ready
  }

  outer.coralSlaveNode.in.foreach { case (in, _) =>
    core.io.axi_slave.write.addr.valid := in.aw.valid
    core.io.axi_slave.write.addr.bits.addr := in.aw.bits.addr(outer.coralnpuParams.axiAddrBits - 1, 0)
    core.io.axi_slave.write.addr.bits.prot := in.aw.bits.prot
    core.io.axi_slave.write.addr.bits.id := in.aw.bits.id
    core.io.axi_slave.write.addr.bits.len := in.aw.bits.len
    core.io.axi_slave.write.addr.bits.size := in.aw.bits.size
    core.io.axi_slave.write.addr.bits.burst := in.aw.bits.burst
    core.io.axi_slave.write.addr.bits.lock := in.aw.bits.lock
    core.io.axi_slave.write.addr.bits.cache := in.aw.bits.cache
    core.io.axi_slave.write.addr.bits.qos := in.aw.bits.qos
    core.io.axi_slave.write.addr.bits.region := in.aw.bits.region
    in.aw.ready := core.io.axi_slave.write.addr.ready

    core.io.axi_slave.write.data.valid := in.w.valid
    core.io.axi_slave.write.data.bits.data := in.w.bits.data
    core.io.axi_slave.write.data.bits.last := in.w.bits.last
    core.io.axi_slave.write.data.bits.strb := in.w.bits.strb
    in.w.ready := core.io.axi_slave.write.data.ready

    in.b.bits := 0.U.asTypeOf(in.b.bits)
    in.b.valid := core.io.axi_slave.write.resp.valid
    in.b.bits.id := core.io.axi_slave.write.resp.bits.id
    in.b.bits.resp := core.io.axi_slave.write.resp.bits.resp
    core.io.axi_slave.write.resp.ready := in.b.ready

    core.io.axi_slave.read.addr.valid := in.ar.valid
    core.io.axi_slave.read.addr.bits.addr := in.ar.bits.addr(outer.coralnpuParams.axiAddrBits - 1, 0)
    core.io.axi_slave.read.addr.bits.prot := in.ar.bits.prot
    core.io.axi_slave.read.addr.bits.id := in.ar.bits.id
    core.io.axi_slave.read.addr.bits.len := in.ar.bits.len
    core.io.axi_slave.read.addr.bits.size := in.ar.bits.size
    core.io.axi_slave.read.addr.bits.burst := in.ar.bits.burst
    core.io.axi_slave.read.addr.bits.lock := in.ar.bits.lock
    core.io.axi_slave.read.addr.bits.cache := in.ar.bits.cache
    core.io.axi_slave.read.addr.bits.qos := in.ar.bits.qos
    core.io.axi_slave.read.addr.bits.region := in.ar.bits.region
    in.ar.ready := core.io.axi_slave.read.addr.ready

    in.r.bits := 0.U.asTypeOf(in.r.bits)
    in.r.valid := core.io.axi_slave.read.data.valid
    in.r.bits.id := core.io.axi_slave.read.data.bits.id
    in.r.bits.data := core.io.axi_slave.read.data.bits.data
    in.r.bits.resp := core.io.axi_slave.read.data.bits.resp
    in.r.bits.last := core.io.axi_slave.read.data.bits.last
    core.io.axi_slave.read.data.ready := in.r.ready
  }
}

class WithNCoralNPUCores(
  n: Int = 1,
  tileParams: CoralNPUTileParams = CoralNPUTileParams()
) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = up(NumTiles)
    (0 until n).map { i =>
      CoralNPUTileAttachParams(
        tileParams = tileParams.copy(
          tileId = i + idOffset,
          coralBaseAddress = tileParams.coralBaseAddress + (BigInt(i) * tileParams.coralWindowBytes)),
        crossingParams = RocketCrossingParams()
      )
    } ++ prev
  }
  case NumTiles => up(NumTiles) + n
})
