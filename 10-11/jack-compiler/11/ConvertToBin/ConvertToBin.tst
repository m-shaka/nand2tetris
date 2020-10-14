load Sys.vm,

set RAM[8000] 100,
set RAM[8001] -1,
set RAM[8002] -1,
set RAM[8003] -1,
set RAM[8004] -1,
set RAM[8005] -1,
set RAM[8006] -1,
set RAM[8007] -1,
set RAM[8008] -1,
set RAM[8009] -1,
set RAM[8010] -1,
set RAM[8011] -1,
set RAM[8012] -1,
set RAM[8013] -1,
set RAM[8014] -1,
set RAM[8015] -1,
set RAM[8016] -1,

repeat 1000 {
    vmstep;
}
