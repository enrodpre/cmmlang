add_test([=[test_token.tokens]=]  /home/kike/dev/cmm/build/test/test_token [==[--gtest_filter=test_token.tokens]==] --gtest_also_run_disabled_tests)
set_tests_properties([=[test_token.tokens]=]  PROPERTIES WORKING_DIRECTORY /home/kike/dev/cmm/build/test SKIP_REGULAR_EXPRESSION [==[\[  SKIPPED \]]==] LABELS unit)
set(  test_token_TESTS test_token.tokens)
