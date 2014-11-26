

C+ENQ_V2_UNITS

       subroutine enq_v2_units ( units, s )
C
C     Returns the name of the external units from control tables.
C
C     Returned:
C         Name of external units.
              character*(*)   units
C         Status
              integer         s
C
C     Control tables version 2 support routine for ENQ_UNITS.
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      if ( s .ne. 0 ) return

      units = Aunits

      end
