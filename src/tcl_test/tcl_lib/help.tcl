proc ? { args } {
  global global.info post_sys
  upvar #0 $post_sys.info info
  upvar #0 global.info ginfo
  if [llength $args] then {
    set com [lindex $args 0]
    foreach name [array names info] {
      if [cmatch $com $name] then {
         puts "$name \t\t\t\t $info($name)"
      }
    }
    foreach name [array names ginfo] {
      if [cmatch $com $name] then {
         puts "$name \t\t\t\t $ginfo($name)"
      }
    }
  } else { 
    foreach name [array names info] {
         puts "$name \t\t\t\t $info($name)"
    }
    foreach name [array names ginfo] {
         puts "$name \t\t\t\t $ginfo($name)"
    }
  }
}



