type button = {
  name : string;
  action : string -> string;
  enabled : bool;
}

let enabled b = b.enabled
