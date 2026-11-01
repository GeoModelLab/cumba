.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
                       |         
        ,---..   .,-.-.|---.,---.
        |    |   || | ||   |,---|
        `---'`---'` ' '`---'`---^
\U0001F345 Carbon Use Model for Yield and Brix Assessment \U0001F345")
}

utils::globalVariables(c("Site", "swc", "waterStressFactor"))
