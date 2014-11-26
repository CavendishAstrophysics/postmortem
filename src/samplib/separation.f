

C+separation

       real*8 function separation( ra1, dec1, ra2, dec2 )

C      Function to determine the angular distance between two points.
C
C     Given:
C         RA and dec of the two points
              Real*8          ra1, dec1, ra2, dec2

C     Returned:
C         Angular separation of the points
C             real*8          Separation
C
C-
      real*8     temp

      temp = dsin(dec1)*dsin(dec2) + dcos(dec1)*dcos(dec2)*dcos(ra1-ra2)

      if ( temp .ge. 1.0D+0 ) then
            separation = 0.0D+0
      else
            separation = dacos( temp )
      end if

      return
      end
