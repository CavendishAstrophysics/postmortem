
C     *****************************************************************
C
c+pbcorr
c
      subroutine pbcorr ( ra, dec,
     *                    ra_aerial, dec_aerial,
     *                    tscope_type,
     *                    pb_value, s                      )
C
C     Function to apply a primary beam correction for a given pointing.
C
C     Given:
C         RA (or hour angle) and dec of the direction that the
C         correction is to be calculated for (angles in radians)
              real*8      ra, dec
C         RA (or hour angle) and dec of the direction that the
C         aerial was pointing at the time
              real*8      ra_aerial, dec_aerial
C         Telescope type
              integer     tscope_type
C
C     Return values:
C         Primary beam correction factor - a value between 0 and 1
              real        pb_value
C         Status value
              integer     s
C
C     Written by Nick Rees,  August 1986
C
C-
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/maplib_minirt.inc'
      include  '/mrao/post/include/lsflib_errors.inc'

      integer     num_models
      parameter ( num_models = 3 )

      integer     lun, i

C     Variables for tabulating and calculating the 151 primary beam.
      real        pb_151(0:30), separation, fract
      data   pb_151  / 1.000,.998,.992,.981,.967,.949,
     *                  .927,.902,.873,.841,.807,.770,
     *                  .730,.690,.647,.604,.559,.515,
     *                  .470,.426,.383,.345,.308,.274,
     *                  .242,.212,.179,.149,.122,.098,.076 /

C     Variables for tabulating and calculating the 38 MHz primary beam.
      logical     setup
      character   pb38_name(num_models)*64
      integer     pb38_rt( minirt_len, num_models )
      integer     pb38_proj( 32, num_models )
      real        pb38_pb( 128*64, num_models )
      real        rtdump(512), prdump(32)
      real*8      uv(2)

      common /prim_beam/  setup, pb38_rt, pb38_proj, pb38_pb
      data    setup       / .false. /
      data    pb38_name   / '(maps-3:38mhz)pb-000070:map',
     *                      '(maps-3:38mhz)pb-040070:map',
     *                      '(maps-3:38mhz)pb-200070:map'   /

C     ****** Main Code **************************************

      if (s.ne.0) return

      if (tscope_type.eq.2        .and.
     *    ra_aerial.lt.0.0D+0     .and.
     *    dabs(dec_aerial/const_d2r-72.0).lt. 1.0D+0 ) then
C         Dec 72 pointing of 38MHz.
          call dpproj( prdump, s )

          if (.not. setup) then
              call dpredt( rtdump, s )

C             Read in the data for the primary beam models.
              do 100, i = 1, num_models
                  call opemap(lun, pb38_name(i),'READ',0,s)
                  call rdredt(lun, 1, 0, s )
                  call dpproj( pb38_proj(1,i), s )
                  call enminirt( pb38_rt(1,i), s )
                  call rdmap( lun, 1, pb38_pb(1,i), s )
                  close( lun )
  100         continue
              call ldredt( rtdump, s )
              setup = (s.eq.0)
          end if
          if (s.ne.0) goto 9999

C         Ascertain which model to use.
          if (ra_aerial.gt.-const_piby4) then
              i = 1
          else if (ra_aerial.gt.-const_piby2) then
              i = 2
          else
              i = 3
          end if

          call ldproj( pb38_proj(1,i), s )
          call rdtouv( ra, dec, uv(1), uv(2), s )
          call ruvval2( pb38_rt(1,i), pb38_pb(1,i), uv, 1, pb_value, s )
          if (pb_value .lt. 0.0 .or. s.ne.0 ) pb_value = 0.0
          s = 0
          call ldproj( prdump, s )
      else
          separation = dsin(dec)*dsin(dec_aerial)+
     *                 dcos(dec)*dcos(dec_aerial)*dcos(ra_aerial-ra)
          if ( separation .gt. 1.0 ) then
              separation = 0
          else if ( separation .lt. -1.0 ) then
              separation = const_pi
          else
              separation = acos( separation )
          end if
C
          if ( tscope_type  .eq. 1 ) then
C             151 MHz telescope primary beam

              if ( separation/const_d2r .ge. 15.0 ) then
                  pb_value = 0.0
              else
                  i = int( separation*2/const_d2r )
                  fract    = separation*2/const_d2r - i
                  pb_value = pb_151(i)-fract*(pb_151(i)-pb_151(i+1))
              end if
          else if ( tscope_type .eq. 2 ) then
C             38 MHz telescope primary beam

              if ( separation/const_d2r .gt. 31.5 ) then
                   pb_value = 0.0
              else
                   pb_value = cos( asin( separation ) / 0.35 )
              end if
          else
              s = ILL_LSF_TSCOPE
          end if
      end if

 9999 if (s.ne.0) call lsf_wrerr( s, 'in subroutine PBCORR' )
      return
      end
