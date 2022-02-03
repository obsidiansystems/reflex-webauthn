{ }:
let
  local-self = import ./. {};
in {
  linuxExe = local-self.linuxExe;
}