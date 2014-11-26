#
# Tcl/Expect procedures to perform Sample-file Tranfers
#
# Paul Alexander, 3/8/93
#
#proc send_spawn {command} {
#  global spawn_id
#  send $command
#
global options 
set options(path) /data4/t151
set options(source) /mrao/post/util
set options(lowercase) 1
set options(host) clfst
set options(user) t151
set options(password) telescope

proc trans_connect { } {
   global options spawn_id nord_prompt
   log_user 0
   set nord_prompt [string toupper $options(host)@]
   spawn telnet $options(host)
   expect *ENTER {exp_send_spawn $options(user)\r}
   expect *PASSWORD: {exp_send_spawn $options(password)\r}
   expect *$nord_prompt
}

proc trans_getlist { sample_file } {
   global options spawn_id nord_prompt output
   set timeout 100
   log_user 0
   exp_send_spawn "list-obs $sample_file terminal\r"
   expect *$nord_prompt {set output $expect_out(0,string)}
   if [string length [info vars output]] then {
     set text [string range $output\
              [expr "[string first \r $output]+2"] \
              [expr "[string first $nord_prompt $output ]-2"]]
     regsub -all \n $text "" output
     set l1 [split $output \r]
     set l2 {}
     foreach i $l1 {
       if [string length $i] then {
         lappend l2 $i
       }
     }
     return $l2
  } else {
     puts "*** No matching sample files"
     return 0
  }
}

proc trans_disconnect { } {
   global spawn_id output
   log_user 0
   exp_send_spawn "logout\r"
}

proc sample_list { sample_file } {
   trans_connect
   set ll [trans_getlist $sample_file]
   trans_disconnect
   return $ll
}

proc get_files { file_list } {
   global options spawn_id
   spawn ftp $options(host)
   log_user 0
   expect {Name*:}    {exp_send_spawn $options(user)\r}
   expect {Password:} {exp_send_spawn $options(password)\r}
   expect {ftp>}      {exp_send_spawn binary\r}
   expect {ftp>}
   set timeout 2400
   foreach file $file_list {
      set i1 [expr "[string first ) $file]+1"]
      set i2 [expr "[string last : $file]-1"]
      set i3 [expr $i2+2]
      set name [string range $file $i1 $i2]
      set type [string range $file $i3 [string length $file]]
      log_user 1
      puts "TRANSFER: $file"
      if $options(lowercase) then {
         set name [string tolower $name]
      }
      set type [string tolower $type]
      exec install -d $options(path)/$name
      set lfile $options(path)/$name/$type
      exp_send_spawn "get \"$file\"  $lfile\r"
      expect ftp>
      puts "TRANSLATE: $lfile"
      case $type in {
         cal  {exec $options(source)/convert_sf $lfile}
         rem  {exec $options(source)/convert_sf $lfile}
         samp {exec $options(source)/convert_sf $lfile}
         s5   {exec $options(source)/convert_sf $lfile}
         s15  {exec $options(source)/convert_sf $lfile}
         s27  {exec $options(source)/convert_sf $lfile}
         s31  {exec $options(source)/convert_sf $lfile}
         ion  {exec $options(source)/convert_ion $lfile}
         lsf  {exec $options(source)/convert_lsf $lfile}
      }
   }
   log_user 0
   exp_send_spawn quit\r
   log_user 1
   puts "Transfer/Translation completed"
}

proc host { host } {
  global options
  case $host in {
     ryle  {set options(host) ryle
            set options(user) ryle
            set options(path) /data4/ryle } 
     clfst {set options(host) clfst
            set options(user) t151
            set options(path) /data4/t151 } 
  }
  return ""
}

proc user { user } {
   global options
   case $user in {
      ryle {set options(user) ryle
            set options(path) /data4/ryle }
      t151 {set options(user) t151
            set options(path) /data4/t151 }
   }
   return ""
}

proc path { path } {
  global options
  set options(path) $path
}

proc match { sample_files } {
  set ll [sample_list $sample_files]
  foreach i $ll {
    puts $i
  }
}

proc recover { sample_file } {
  set ll [sample_list $sample_file]
  get_files $ll
}

proc lowercase { onoff } {
  global options
  case $onoff in {
    on  { set options(lowercase) 1 }
    off { set options(lowercase) 0 }
  }
}

proc quit { } {exit}

proc q { } {exit}


