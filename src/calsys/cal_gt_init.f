C
C+cal_gt_init

       subroutine cal_gt_init( s )
C      ---------------------------
C
C Initialise the current gains table
C
C      Returned:
C         error status
                  integer      s
C
C Initialise the current gains table if it is not already initialised.
C The gains table is initialised to (1.0,0.0).
C
C PA 25/04/90
C PA 05/02/92; added support for multiple GT files to be defined and
C              definition of the file names from C.L. parameters.
C-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/post_sys_pars.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_control.inc'

C Local variables
C    counters
       integer     iae, isb, ich, i


       if (s.ne.0) return

C check the gains-table file names and if not set use:
C  (a) file names defines in the command language
C  (b) default file names
       if (chr_lenb(RT_gt_file).eq.0) then
c        call cmd_enqparam('RT-GT-file',RT_gt_file,s)
         if (s.ne.0) s = 0
         if (chr_lenb(RT_gt_file).eq.0) then
           RT_gt_file = def_RT_gt_file
         end if
       end if
       if (chr_lenb(RT_gtvis_file).eq.0) then
c        call cmd_enqparam('RT-GTvis-file',RT_gtvis_file,s)
         if (s.ne.0) s = 0
         if (chr_lenb(RT_gtvis_file).eq.0) then
           RT_gtvis_file = def_RT_gtvis_file
         end if
       end if

C initialise the current gains table
       if ((.not.current_gains_table) .and.
     *     (.not.current_solution)         ) then
         do i=1,max_vis
           vis_gains(i) = (1.0,0.0)
         end do
         do iae=1,max_RT_aes
           do isb=1,max_subb
             do ich=1,max_channel
               ae_gains(ich,isb,iae) = (1.0,0.0)
             end do
           end do
         end do
         current_refant = -1
       else if (current_solution .and. (.not.current_gains_table)) then
         current_refant = cal_refant
       end if

       if (s.ne.0) call cal_wrerr(s,'in cal_gt_init')

       end
