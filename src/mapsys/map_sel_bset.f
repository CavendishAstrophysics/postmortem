
C     *****************************************************************
C
C+map_sel_bset
C
      SUBROUTINE map_sel_bset ( s )

C     Asks the user to select the map beamset parameters.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the beamset parameters of the map.
C         modified by PJW to include integration time smearing 8/91
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/iolib_functions.inc'
      include    '/mrao/include/maplib_redtape.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Local copies of the beamset parameters.
              character*3 def_bw, def_pb, def_integ
              integer     du_set, dv_set, u0_set, v0_set, nu_set, nv_set
              logical     bw_smear, pb_corr, integ_smear
              logical     btest
              intrinsic   btest
              intrinsic   ibset

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if (maptyp .eq. 3) then
          du_set = duset
          dv_set = dvset
          def_bw = 'No'
          def_integ = 'No'
          def_pb = 'No'
          if (btest(bsetfw,1)) def_bw = 'Yes'
          if (btest(bsetfw,3)) def_integ = 'Yes'
          if (btest(bsetfw,2)) def_pb = 'Yes'
      else
          du_set = 60
          dv_set = 60
          def_pb = 'Yes'
          def_integ = 'Yes'
          def_bw = 'Yes'
      end if

      call io_geti('Distance between beams in u  : ', '*', du_set, s)
      if (du_set.ne.duset) dv_set = du_set
      call io_geti('Distance between beams in v  : ', '*', dv_set, s)
      du_set = max( 1, abs(du_set) )
      dv_set = max( 1, abs(dv_set) )

  100 continue
        u0_set = iumap1  + ixmax/2 - int((ixmax/2)/du_set)*du_set
        call io_geti('Lowest u coordinate of beams : ', '*', u0_set, s)
        if (u0_set.lt.iumap1.or.u0_set.gt.iumap2)
     *                    call io_wrout( 'Coordinate must be on map.' )
        if (u0_set.lt.iumap1.or.u0_set.gt.iumap2) goto 100

  200 continue
        v0_set = ivmap2-1+ iymax/2 - int((iymax/2)/dv_set)*dv_set
        call io_geti('Lowest v coordinate of beams : ', '*', v0_set, s)
        if (v0_set.gt.ivmap1.or.v0_set.lt.ivmap2)
     *                    call io_wrout( 'Coordinate must be on map.' )
        if (v0_set.gt.ivmap1.or.v0_set.lt.ivmap2) goto 100

      nu_set = int((iumap2-u0_set)/du_set ) + 1
      nv_set = int((ivmap1-v0_set)/dv_set ) + 1
      call io_geti('Number of beams along u axis : ', '*', nu_set, s)
      nu_set = max(1,min(abs(nu_set), int((iumap2-u0_set)/du_set )+1 ))
      call io_geti('Number of beams along v axis : ', '*', nv_set, s)
      nv_set = max(1,min(abs(nv_set), int((ivmap1-v0_set)/dv_set )+1 ))

C     Set up model definition
C      call enq_point( phys_sf_lun, mod_ra_aes, mod_dec_aes, s )
C      call enq_tscope( phys_sf_lun, string, mod_tscope, s )
C      call enq_freq( phys_sf_lun, frequency, s )

C      if (mod_tscope.eq.1) then
C     151.5 implies .8Mhz : 151 sets .7
C          mod_band_type  = 1
C          if ( frequency .eq. 151.5d+6 ) then
C                mod_band_width = 0.8D+6/frequency
C          else
C                mod_band_width = 0.7D+6/frequency
C          endif
C      else if (mod_tscope.eq.2) then
C          mod_band_type  = 2
C          mod_band_width = 0.127D+6/frequency
C      else
C          mod_band_type  = 0
C          mod_band_width = 0.0
C      end if

      if ( io_yesno( 'Apply bandwidth smearing ? ', def_bw, s ) ) then
        bw_smear = .true.
        call io_getr('bandwidth in Hz!! :','*', effective_bandwidth, s ) 
      else
        bw_smear = .false.
      endif
      if ( io_yesno( 'Apply integration-time smearing ?',
     *                     def_integ, s ) ) then
        integ_smear = .true.
        call io_getr( 'integration-time (secs):', '*',
     *               effective_integration, s )
      else
        integ_smear = .false.
      endif
      pb_corr = io_yesno( 'Apply primary beam correction ?', def_pb, s )

      if (s.eq.0) then
          maptyp = 3
          bsetid = 'BSET'
          bsetfw = 0
          bsetfw = ibset( bsetfw, 0 )
          if (bw_smear) bsetfw = ibset( bsetfw, 1 )
          if (pb_corr) bsetfw = ibset( bsetfw, 2 )
          if (integ_smear) bsetfw = ibset( bsetfw, 3 )
          u0set  = u0_set
          v0set  = v0_set
          duset  = du_set
          dvset  = dv_set
          nuset  = nu_set
          nvset  = nv_set
      end if

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine MAP_SEL_BSET' )
          return
      end
