package chipyard

import org.chipsalliance.cde.config.Config

// ---------------------
// CoralNPU Configs
// ---------------------

class CoralNPUConfig extends Config(
  new chipyard.WithNCoralNPUCores(1) ++
  new chipyard.config.AbstractConfig)
