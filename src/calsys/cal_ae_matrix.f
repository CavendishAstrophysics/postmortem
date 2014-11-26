C+ cal_ae_matrix

       subroutine cal_ae_matrix ( mode, m, n, x, y, rw, lrw, iw, liw )
C      ---------------------------------------------------------------
C
C Internal routine to perform specified matrix operations for F04QAF
C
C Calling sequence is defined in the NAG reference manual as are the
C matrix operations performed by this routine.  The matrix array must
C have been initialised by a call to cal_init_soln, but as this routine
C is only called internally by cal_calc_soln which checks for correct
C initialisation no further checks are required here.
C
*-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_ae_matrix.inc'

       integer   mode, m, n, lrw, liw
       real*8    x(n), y(m), rw(lrw)
       integer   iw(liw)

       integer   im, in

       if (calculate_amp_solution) then
         if (mode.eq.1) then
           do im=1,m
             y(im) = y(im) + x(mat(1,im)) + x(mat(2,im))
           end do
         else if (mode.eq.2) then
           do in=1,n
             do im=1,tmat(0,in)
               x(in) = x(in) + y(abs(tmat(im,in)))
             end do
           end do
         end if

       else
         if (mode.eq.1) then
           do im=1,m
             y(im) = y(im) + x(mat(1,im)) - x(mat(2,im))
           end do
         else if (mode.eq.2) then
           do in=1,n
             do im=1,tmat(0,in)
               if (tmat(im,in).lt.0) then
                 x(in) = x(in) - y(abs(tmat(im,in)))
               else if (tmat(im,in).gt.0) then
                 x(in) = x(in) + y(abs(tmat(im,in)))
               end if
             end do
           end do
         end if

       end if

       end
