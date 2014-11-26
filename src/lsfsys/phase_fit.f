
C+ phase_fit

        subroutine phase_fit (s)
*
        integer s       ! status


* sets phase-fit on or off for plot_complexN

C-

        logical io_onoff, switch

        if (s .ne. 0) return

        switch = io_onoff ('Phase-fitting on or off :', 'OFF', s)

        if (switch) then
                call plot_set_fit ('YES', s)
        else
                call plot_set_fit ('NO',  s)
        endif
        return

        end

