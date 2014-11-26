*+
      subroutine flag_shad_ryle(file,lsf_key,status)
C     ----------------------------------------------
C
C  Executes the SHADOW command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      LSF_KEY       integer       current logical sample file key
C      STATUS        integer       status value
C
C This subroutine calculates what portions of a sample file have suffered
C from geometrical shadowing and prepares appropriate flag table entries
C
C Keith Grainge 21/11/95
C
*-
      character  file*(*)
      integer    lsf_key, status

      integer ha_beg_int(5,3),ha_end_int(5,3)
      integer run_beg_int(3),run_end_int(3)
      logical shadow_beg(5),shadow_end(5)

              call calc_shadow(file, shadow_beg, shadow_end,
     &          ha_beg_int, ha_end_int,run_beg_int,run_end_int,
     &           status)

              call shadow_flag(file, shadow_beg, shadow_end,
     &          ha_beg_int, ha_end_int,run_beg_int,run_end_int,
     &          lsf_key,status)


      end
