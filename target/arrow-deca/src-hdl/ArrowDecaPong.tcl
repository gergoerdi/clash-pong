if {$argc < 1} {
    puts stderr "Usage: quartus_sh -t $argv0 <output dir>"
    puts stderr "ERROR: Not enough arguments. Please supply an output directory."
    exit 1
}

set output_dir [lindex $argv 0]
set output_dir [file normalize $output_dir]
puts stderr "Output directory: $output_dir"

project_open [file join $output_dir ArrowDecaPong.qpf]

set_global_assignment -name PROJECT_OUTPUT_DIRECTORY [file normalize $output_dir]

package require ::quartus::flow
execute_flow -compile
