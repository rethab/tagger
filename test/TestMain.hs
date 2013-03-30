import Test.Framework (defaultMain, testGroup)

import TestCrawler (tests)
import TestIncompletionFinder (tests)
import TestTagCompleter (tests)
import TestTagWriter (tests)

main = defaultMain
    [ testGroup "Crawler" TestCrawler.tests
    , testGroup "IncompletionFinder" TestIncompletionFinder.tests
    , testGroup "TagCompleter" TestTagCompleter.tests
    , testGroup "TagWriter" TestTagWriter.tests
    ]
