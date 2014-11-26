*+ flag_add_entry

       subroutine flag_add_entry( flag_id, lsf_num, sf_lun,
     *                            program, status         )
C      ----------------------------------------------------
C
C Construct and add an entry to the flag table
C
C Given:
C   flag file identifier
       integer         flag_id
C   logical sample file number
       integer         lsf_num
C   sample file logical unit number
       integer         sf_lun
C   program name creating entry
       character*(*)   program
C Updated:
C   error status code
       integer         status
C
C A new entry for the flag table is constructed, prompting the
C user as required.  All components of the entry must be quoted,
C components may be separated with the '%' character on the command
C line. The interaction requests:
C   version to write to
C   spacing-list
C   sample-range
C   operation
C   comment
C
C [PA, 14/8/91] [GP: defaults changed, 15/9/93]
C-

       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer         numsamp
       integer         version
       character*80    spac_list, time_range, comment
       character*10    operation
       character*16    text, default
       character*128   line, string
c      logical         cmd_dblev

       if (status.ne.0) return

C read version
       call io_geti('Version to write to (-1=new-version) : ','-1',
     *           version, status)
       if (status.ne.0) goto 999

C read spacing list
       line = ' '
       call io_getstr('Spacing-list : ',' ',line,status)
       spac_list = line(chr_intlc(line):chr_lend(line,'%'))
       if (chr_lend(line,'%').eq.chr_lenb(line)) then
         line = ' '
       else
         string = line(chr_lend(line,'%')+2:chr_lenb(line))
         line = string
       end if

C read time range
       if (chr_lenb(line).eq.0) then
         call enq_numsamp (sf_lun, 1, numsamp, status)
         write (text, '(i6)') numsamp
         default = '1-'//text(chr_intlc(text):chr_lenb(text))
         call io_getstr('Sample-range : ',default,line,status)
       end if
       time_range = line(chr_intlc(line):chr_lend(line,'%'))
       if (chr_lend(line,'%').eq.chr_lenb(line)) then
         line = ' '
       else
         string = line(chr_lend(line,'%')+2:chr_lenb(line))
         line = string
       end if

C read operation
       if (chr_lenb(line).eq.0) then
         call io_getstr('set or unset : ','SET',line,status)
       end if
       operation = line(chr_intlc(line):chr_lend(line,'%'))
       if (chr_lend(line,'%').eq.chr_lenb(line)) then
         line = ' '
       else
         string = line(chr_lend(line,'%')+2:chr_lenb(line))
         line = string
       end if

C read comment
       if (chr_lenb(line).eq.0) then
         call io_getstr('Comment : ',' ',line,status)
       end if
       comment = line(chr_intlc(line):chr_lend(line,'%'))
       if (chr_lend(line,'%').eq.chr_lenb(line)) then
         line = ' '
       else
         string = line(chr_lend(line,'%')+2:chr_lenb(line))
         line = string
       end if

C add entry to table
c      if (cmd_dblev(4)) then
c        print *,'version=',version
c        print *,'program=',program(1:chr_lenb(program))
c        print *,'spac_list=',spac_list(1:chr_lenb(spac_list))
c        print *,'time_range=',time_range(1:chr_lenb(time_range))
c        print *,'operation=',operation(1:chr_lenb(operation))
c        print *,'comment=',comment(1:chr_lenb(comment))
c      end if
       call flag_write_entry( flag_id, lsf_num, sf_lun, version,
     *                        program(1:chr_lenb(program)),
     *                        spac_list(1:chr_lenb(spac_list)),
     *                        time_range(1:chr_lenb(time_range)),
     *                        operation(1:chr_lenb(operation)),
     *                        comment, status )

999    call flag_err( status, 'FLAG_ADD_ENTRY', 'Failed' )
       end
