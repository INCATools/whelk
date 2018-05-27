appender("SystemErr", ConsoleAppender) {
    target = 'System.err'
    encoder(PatternLayoutEncoder) {
        pattern = "%d %level %logger - %msg%n"
    }
}

appender("FILE", FileAppender) {
    file = "whelk.log"
    append = true
    encoder(PatternLayoutEncoder) {
        pattern = "%d %level %logger - %msg%n"
    }
}

root(INFO, ["SystemErr", "FILE"])

logger("org.semanticweb.owlapi", ERROR)

logger("uk.ac.manchester.cs.owl.owlapi", ERROR)
