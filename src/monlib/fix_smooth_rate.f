



      subroutine fix_smooth_rate(offset_time, status)
*     -----------------------------------------------

* given:  offset_time integer samples/data point in 5-point cycle
*         status      integer zero on entry

* plants this value in smoothing, sample-rate vars in common block

      integer offset_time, status

       include '/mrao/post/include/lsf_definition.inc'
       include '/mrao/post/include/lsf_runtime.inc'

      if (status .ne. 0) return

          smooth_size = offset_time
          samp_rate   = offset_time
          curr_samp   = 0          ! old sample, buffer invalid
          curr_buff   = 0
          start_buff  = 0

      end
