# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "c:\home\tests",
            To_File  => "c:\home\tests\mysite\site\d1\idx.txt") = mysite\site\d1\idx.txt

2. Case sensitivity : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "C:\USERS\Xavier\PrOJ") = ..\Xavier\PrOJ

3. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite\site\d1\idx.txt

4. subdir with Prefix : OK
Short_Path (From_Dir => "\home\tests",
            To_File  => "\home\tests\mysite\site\d1\idx.txt",
            Prefix   => ".\") = .\mysite\site\d1\idx.txt

5. Sibling subdir : OK
Short_Path (From_Dir => "\home\tests\12\34",
            To_File  => "\home\tests\mysite\site\d1\idx.txt") = ..\..\mysite\site\d1\idx.txt

6. Parent dir : OK
Short_Path (From_Dir => "\home\tests\12\34",
            To_File  => "\home\tests\idx.txt") = ..\..\idx.txt

7. Other Prefix : OK
Short_Path (From_Dir => "\home\tests\12\",
            To_File  => "\home\tests\mysite\site\d1\idx.txt",
            Prefix   => "$PWD\") = $PWD\..\mysite\site\d1\idx.txt

8. Root dir : OK
Short_Path (From_Dir => "d:",
            To_File  => "d:\home\tests\mysite\site\d1\idx.txt") = \home\tests\mysite\site\d1\idx.txt

9. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt

10. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt

11. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\

12. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\

13. No common part : OK
Short_Path (From_Dir => "\home\toto\src\tests\",
            To_File  => "\opt\GNAT\2018\lib64\libgcc_s.so") = \opt\GNAT\2018\lib64\libgcc_s.so

14. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

15. Windows Path : OK
Short_Path (From_Dir => "x:",
            To_File  => "Xavier\Proj") = Xavier\Proj

16. Windows Path, not on the same drive : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "d:\Users\Xavier\Proj") = d:\Users\Xavier\Proj

17. Windows Path, case sensitivity : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "C:\USERS\Xavier\PrOJ") = ..\Xavier\PrOJ

18. UNC Path : OK
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\Xavier\Proj

19. Mixing both separator uses : OK
Short_Path (From_Dir => "c:/Users/Lionel/tmp",
            To_File  => "c:\Users\Lionel\Proj\truc") = ..\Proj\truc


File_Utilities.Short_Path tests OK
