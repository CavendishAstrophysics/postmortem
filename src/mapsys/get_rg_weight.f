
C+get_rg_weight

      subroutine get_rg_weight(   num_vis,
     &                             wghttp,
     &                             lsf_num,
     &                             max_vi,
     &                             samp_aes,
     &                             Tsys, buff_num,
     &                             nominal_rg,
     &                             weight,
     &                             rg_weight,
     &                             s )

C     Modifies the weight buffer for the current visibility according to
C     the rain gauge readings, and updates the overall weight of the
C     aperture.
C     from MEJ (1992) version on CLFST:  GP 9 May 2000

C     Given
C         Number of visibilities in the buffer
             integer     num_vis
C         Weight type: values 20-30 indicate rg weighting required
             integer     wghttp
C         Lsf number being used
             integer     lsf_num
C         Max number of visibilities, needed for array declaration below
              integer    max_vi
C         Array of aerial pairs for each visibility in the buffer
             integer     samp_aes( max_vi, 2 )
C         Array of nominal system temperatures of each aerial
             real        Tsys( 1 )
C         Current buffer number
             integer     buff_num
C         rg value for which actual Tsys = nominal Tsys
             real        nominal_rg
C         Array of weights already set by get_weight_buff
             real        weight( num_vis )
C     Returns
C         Array of weights for the current visibility buffer
C             real        weight( num_vis )
C         Accumulated sum of weights for normalising aperture
             real        rg_weight
C         Status - must be zero on entry
             integer     s
C-

      implicit none

      include '/mrao/post/include/phys_tscopes.inc'
      include '/mrao/post/include/global_constants.inc'
      include '/mrao/post/include/mon_v2_block.inc'


      integer lun, lsf_source, itscope
      integer i, j, first_sample, last_sample
      real rain
      real wt, rg(max_rt_aes)

      if (s.ne.0) goto 999

      call lsf_enq_sf(lsf_num, lun, lsf_source, s)
      call enq_sftype(lun, i, s)
      call enq_phys_tscope(lun, itscope, s)
      if (itscope.ne.ryle) return

C     Check if rg weighting required
      if ((wghttp.lt.20).or.(wghttp.gt.29)) return

C     Get range of physical samples in this buffer
      call lsf_enq_samples(lsf_num, buff_num,
     &                 first_sample, last_sample, s)

C     Calculate average rg values during buffer
      do 100, i = 1, max_rt_aes
          rg(i) = 0.0
          do 200, j = first_sample, last_sample
              call read_monitor(lun, j, mon_length, mon_block, s)
              if (s.ne.0) then
                    call map_wrerr(s, 'in subroutine GET_RG_WEIGHT')
              end if
              call enq_mon_rain(lun, i, rain, s)
              rg(i) = rg(i) + rain
 200      continue
          rg(i) = rg(i) / (last_sample - first_sample + 1)
 100  continue

C     Calculate and accumulate weights
C     Weight is 1 / (calculated Tsys1 * Tsys2)

      do 300, i = 1, num_vis
          wt = rg(samp_aes(i,1)) * rg(samp_aes(i,2))
     &    / (Tsys(samp_aes(i,1)) * Tsys(samp_aes(i,2))
     &    * nominal_rg * nominal_rg)
          weight(i) = weight(i) * wt
          rg_weight = rg_weight + wt
 300  continue

      return

 999  call map_wrerr(s, 'in subroutine GET_RG_WEIGHT')

      end



