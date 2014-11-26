#
# Create new minimal-match commands
#
proc command_create { name args } {
  global  post_sys
  set sys $post_sys
  set command {}
  set info {}
  for {set n 0} {$n < [llength $args]} {incr n} {
    set m [lindex $args $n]
    case $m in {
        {-system}  {incr n ; set sys [lindex $args $n]}
       {-global}  {set sys global}
       {-local}   {set sys $anmap_sys}
       {-command} {incr n ; set command [lindex $args $n]}
       {-info}    {incr n ; set info [lindex $args $n]}
    }
  }

# create variables in global context
  upvar #0 $sys.commands com
  upvar #0 $sys.info inf

# update command string
  set com($name) "$command"
  set inf($name) "$info"

}

#
# Provide external application launching
#
proc post_external { application } {
  iocmd save
  post_system $application
  iocmd recover
}

