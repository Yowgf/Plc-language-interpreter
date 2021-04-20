SET cur_dir=%CD%
SET input_file=%1

CD /D "%~dp0"
CD ./src

if defined input_file (
    sml start.sml %cur_dir%/%input_file%
) else (
    sml start.sml
)