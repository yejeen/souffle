git clone https://github.com/microsoft/vcpkg
call .\vcpkg\bootstrap-vcpkg.bat
vcpkg\vcpkg install getopt:x64-windows
setx /M PATH "%PATH$%;%cd%\vcpkg\installed\x64-windows\bin"
call refreshenv
