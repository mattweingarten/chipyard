package chipyard

import org.chipsalliance.cde.config.Config
import nanopuwrapper.WithNanoPU

// ---------------------
// CoralNPU Configs
// ---------------------

class CoralNPUConfig extends Config(
  new WithNanoPU(
    address = 0x70000000L,
    size = 0x400000L,
    implementationTag = "reference"
  ) ++
  new chipyard.config.AbstractConfig)
