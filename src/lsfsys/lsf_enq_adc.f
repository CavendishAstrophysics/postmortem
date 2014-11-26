C
C
C+lsf_enq_adc

       subroutine lsf_enq_adc( enq_ratel, enq_adc, s )
C
C return the values of the constants found after a phase fit
C
C Returned:
C    the value of RATEL used to calculte HA
       real*4     enq_ratel
C    the fitting constants A, D and C
       real*4     enq_ADC(3)
C    status word
       integer    s
C
C PA, 10/10/90
C-

       integer                i
       real*4                 save_ratel, save_ADC(3)
       common /save_fit_res/  save_ratel, save_ADC
       if (s.ne.0) return

       enq_ratel = save_ratel
       do i=1,3
         enq_ADC(i) = save_ADC(i)
       end do

       if (s.ne.0) call io_wrerr(s,'in lsf_enq_ADC')
       end
