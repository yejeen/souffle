git clone https://github.com/microsoft/vcpkg
call .\vcpkg\bootstrap-vcpkg.bat
vcpkg\vcpkg install getopt:x64-windows
setx /M PATH "%PATH$%;%cd%\vcpkg\installed\x64-windows\bin"
setx /M INCLUDE "%INCLUDE%;%cd%\vcpkg\installed\x64-windows\include;"
setx /M LIB "%LIB%;%cd%\vcpkg\installed\x64-windows\lib"
refreshenv
echo %PATH%
echo %INCLUDE%
echo %LIB%
