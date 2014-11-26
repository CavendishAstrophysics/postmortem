

*+PRINT_LIST

       subroutine print_list( list, nlist, status )
C      --------------------------------------------
C
C Print a formatted list.
C
C Given
C        List
       integer       list(1)
C        Number of items in the list
       integer       nlist
C Updated
C        Error code
       integer       status

C local variables, loop counters, output device and max in list
       integer       n, iout, list_max
C length of integer, line, number of of items on line, number of lines
       integer       length_integer, line_length, number_on_line,
     *               number_lines
C output format statement to be constructed
       character     format_statement*80
       integer       len_format
C integer functions
       integer       chr_lenb
C
C PA, 26/7/89
C
C ======================================================================


C check status on entry
       if (status.ne.0) return

C find output device
       call io_enqout(iout)

C find maximum in the list
       list_max = 0
       do n = 1,nlist
         list_max = max ( list_max, abs(list(n)) )
         if (list(n).lt.0) list_max = list_max*10
       end do

C format depends on the maximum
       if (list_max.lt.10000) then
         length_integer = 5
       else if (list_max.lt.1000000000) then
         length_integer = 10
       end if

C set output format
       line_length    = 80
       number_on_line = line_length/length_integer
       number_lines   = nlist/number_on_line + 1
       if (length_integer.eq.5) then
         write (format_statement,1) number_lines
  1      format ('(',I2,'(1X,16I5/1X))')
       else if (length_integer.eq.10) then
         write (format_statement,2) number_lines
  2      format ('(',I2,'(1X,8I10/1X))')
       end if
       len_format = chr_lenb(format_statement)

C print list to output device
       write (iout,fmt=format_statement(1:len_format))
     *       (list(n), n=1,nlist)
       write (iout,*)' '

C check status on exit
       if (status.ne.0) then
         call mon_wrerr( status, 'in subroutine PRINT_LIST' )
       end if

       end
