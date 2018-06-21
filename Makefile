all:
	jbuilder build @all --display=short

b=./_build/default
demo: all
	$(b)/dyn_test.exe
	$(b)/cavalry_read.exe int_box.bin
	$(b)/cavalry_read.exe string_box.bin
	$(b)/dyn_test.exe int_box.bin
	$(b)/dyn_test.exe string_box.bin
	LOAD_PLUGIN=1 $(b)/dyn_test.exe
	! $(b)/dyn_test.exe plugin_box.bin
	LOAD_PLUGIN=1 $(b)/dyn_test.exe plugin_box.bin
	! LOAD_PLUGIN=2 $(b)/dyn_test.exe

clean:
	jbuilder clean
	rm -f *.bin
