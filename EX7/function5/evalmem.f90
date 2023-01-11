module evalMem
    implicit none  
contains

subroutine system_mem_usage(mem_string)
    implicit none
    character(len=80) :: line
    character(len=8)  :: junk
    character(len=5), intent(out) :: mem_string
  
    !define bash command
    line = trim("top  -o mem -l 1 | grep 'main'  > mem.txt")
    call execute_command_line (line)

    !save line output to file
    open(1, file='mem.txt', status='old')

    !extract memory from file
    read(1, *) junk, junk, junk, junk, junk, junk, junk , mem_string

    !remove file
    call execute_command_line ('rm mem.txt')
    end subroutine system_mem_usage
    
end module evalMem