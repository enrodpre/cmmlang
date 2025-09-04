add_test([=[AstTest.operator_precedence]=]  /home/kike/dev/cmm/build/test/test_ast [==[--gtest_filter=AstTest.operator_precedence]==] --gtest_also_run_disabled_tests)
set_tests_properties([=[AstTest.operator_precedence]=]  PROPERTIES WORKING_DIRECTORY /home/kike/dev/cmm/build/test SKIP_REGULAR_EXPRESSION [==[\[  SKIPPED \]]==] LABELS unit)
set(  test_ast_TESTS AstTest.operator_precedence)
