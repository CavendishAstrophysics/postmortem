

C+ cal_init_soln

       subroutine cal_init_soln( m, n, ia, nn, s )
C      -------------------------------------------
C
C Initialise aerial arrays for solution of telescope gains
C
C Given:
C   number of visibilities
       integer     m
C   number of aerials
       integer     n
C Updated:
C   aerial list for each visbility
       integer*2   ia(2,m)
C Returned:
C   number of non-redundant aerials
       integer     nn
C   error status
       integer     s
C
C The aerial matrix array and its transpose are initialised.  The aerial
C matrix (and transpose) are to be considered as matrices based on aerial
C index numbers, where the aerials participating in the solution are
C numbered 1 to number_of_aerials_in_solution.  The number of aerials used
C is returned to the calling program.
*-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_ae_matrix.inc'

       integer*2   index_list(max_aes)

C local variables (counters)
       integer   i, j, ii

C check status on entry
       if (s.ne.0) return

C initialise the index array between actual aerials and those to be used
C in the solution
       do j=1,n
         index_list(j) = 0
         ae_list(j) = 0
       end do
C construct index array
       do i=1,m
         index_list(ia(1,i)) = ia(1,i)
         index_list(ia(2,i)) = ia(2,i)
       end do
       nn = 0
       do j = 1,n
         if (index_list(j).gt.0) then
           nn = nn + 1
           index_list(j) = nn
           ae_list(nn) = j
         end if
       end do

C define the aerial matrix
       do i=1,m
         do j = 1,2
           mat(j,i) = index_list(ia(j,i))
         end do
       end do

C construct the transpose matrix, again in terms of index numbers
C .. the matrix is not fully represented but is held in redundant form
C .. (i.e. only non-zero elements are referenced).  TMAT(0,n) is the
C .. number of non-zero entries in row n.  sign(1,TMAT(i,n))
C .. gives the matrix element value of row n column abs(TMAT(i,n)).
       do j=1,nn
         tmat(0,j) = 0
         do i=1,m
           if (mat(1,i).eq.j) then
             tmat(0,j) = tmat(0,j) + 1
             ii = tmat(0,j)
             tmat(ii,j) = i
           end if
           if (mat(2,i).eq.j) then
             tmat(0,j) = tmat(0,j) + 1
             ii = tmat(0,j)
             tmat(ii,j) = -i
           end if
         end do
       end do

C set flag indicating that the aerial matrix has been initialised
       matrix_initialised = .true.

C change cal_refant to an aerial index value
       refant = index_list( cal_refant )

C report any errors
       if (s.ne.0) call cal_wrerr( s, 'in cal_init_soln' )

       end
