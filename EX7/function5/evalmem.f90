module evalMem
    implicit none  
contains

subroutine system_mem_usage(mem_string)
    implicit none
    !real, intent(out) :: mem
    character(len=80) :: line
    character(len=8)  :: junk
    character(len=5), intent(out) :: mem_string
  
    !This is horrible but it works
   
    line = trim("top  -o mem -l 1 | grep 'main'  > mem.txt")
    call execute_command_line (line)

    !save line of top to file
    open(1, file='mem.txt', status='old')

    !extract memory
    read(1, *) junk, junk, junk, junk, junk, junk, junk , mem_string

    !remove file
    call execute_command_line ('rm mem.txt')
    return
    end subroutine system_mem_usage
    
end module evalMem