export interface Overview {
  cpu: CPU;
  mem: Memory;
  net: Network;
  disk: Disk;
  process: Process;
}

export interface CPU {
  load: number;
  cores: number[];
  processes: number;
  procsRunning: number;
  procsBlocked: number;
  boottime: number;
  switchContext: number;
}

export interface Memory {
  total: number;
  free: number;
  available: number;
  occupied: number;

  swapTotal: number;
  swapFree: number;
  swapOccupied: number;

  vmAllocTotal: number;
  vmAllocFree: number;
  vmAllocOccupied: number;
}

export interface Network {
  interfaces: NetworlInterface[];
}

interface NetworlInterface {
  name: string;
  upload: number;
  download: number;
}

export interface Disk { }

export interface Process { }
