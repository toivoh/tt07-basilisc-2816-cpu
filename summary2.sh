
echo "Linter output"
echo "============="
cat runs/wokwi/01-verilator-lint/verilator-lint.log
echo

echo "Synthesis warnings"
echo "=================="
grep "Warning" runs/wokwi/06-yosys-synthesis/yosys-synthesis.log
echo

echo "Cell usage"
echo "=========="
grep "Number of" runs/wokwi/06-yosys-synthesis/yosys-synthesis.log | tail -n 8
#grep "dfxtp_.  " runs/wokwi/06-yosys-synthesis/yosys-synthesis.log
./tt/tt_tool.py --openlane2 --print-cell-summary | grep dfxtp
./tt/tt_tool.py --openlane2 --print-cell-summary | grep Total
grep "Chip area" runs/wokwi/06-yosys-synthesis/yosys-synthesis.log
echo

echo "Utilization"
echo "==========="
grep Util runs/wokwi/*-openroad-globalplacement/openroad-globalplacement.log
echo

echo "STA"
echo "==="
grep "timing__setup__ws\"" runs/wokwi/67-misc-reportmanufacturability/state_out.json
grep "timing__hold__ws\"" runs/wokwi/67-misc-reportmanufacturability/state_out.json
echo
grep "design__max_slew_violation__count\"" runs/wokwi/67-misc-reportmanufacturability/state_out.json
grep "design__max_fanout_violation__count\"" runs/wokwi/67-misc-reportmanufacturability/state_out.json
grep "design__max_cap_violation__count\"" runs/wokwi/67-misc-reportmanufacturability/state_out.json
echo
