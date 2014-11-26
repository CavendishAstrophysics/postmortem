
C+ENQ_V2_CHGEOM

       subroutine enq_v2_chgeom ( iae, iba, ich, xyz_coord, s )
C
C     Returns the geometry for the specified aerial and channel
C
C     Given:
C         aerial number
              integer         iae
C         sub-band number
              integer         iba
C         channel number
              integer         ich
C
C     Returned:
C         Coordinates of the telescope/band/channel, in wavelengths
              real*8          xyz_coord(3)
C         Status
              integer         s

C     PA, 28/2/89
C     DJT, 20/2/90
*-

       integer         i, j, k, ii, jj, kk

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

       if (s.ne.0) return

       ii = 0
       jj = 0
       kk = 0
       do i = 1, Nchannel
          if (ich.eq.ich_code(i)) ii = i
       enddo
       do j = 1, Nsubb
          if (iba.eq.iba_code(j)) jj = j
       enddo
       do k = 1, Naes
          if (iae.eq.iae_code(k)) kk = k
       enddo

       xyz_coord(1) = 0.0d0
       xyz_coord(2) = 0.0d0
       xyz_coord(3) = 0.0d0

       if (ii.eq.0) then
         s = ILL_CHANNEL
         goto 999
       else if (jj.eq.0) then
         s = ILL_SUBBAND
         goto 999
       else if (kk.eq.0) then
         s = ILL_AERIAL
         goto 999
       else
         xyz_coord(1) = x(ii,jj,kk)
         xyz_coord(2) = y(ii,jj,kk)
         xyz_coord(3) = z(ii,jj,kk)
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_CHGEOM' )

       end
