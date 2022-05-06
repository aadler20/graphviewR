_P_a_r_s_e _a_n _R_d _F_i_l_e

_D_e_s_c_r_i_p_t_i_o_n:

     This function reads an R documentation (Rd) file and parses it,
     for processing by other functions.

_U_s_a_g_e:

     parse_Rd(file, srcfile = NULL, encoding = "unknown",
              verbose = FALSE, fragment = FALSE, warningCalls = TRUE,
              macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"),
              permissive = FALSE)
     ## S3 method for class 'Rd'
     print(x, deparse = FALSE, ...)
     ## S3 method for class 'Rd'
     as.character(x, deparse = FALSE, ...)
     
_A_r_g_u_m_e_n_t_s:

    file: A filename or text-mode connection.  At present filenames
          work best.

 srcfile: 'NULL', or a '"srcfile"' object.  See the 'Details' section.

encoding: Encoding to be assumed for input strings.

 verbose: Logical indicating whether detailed parsing information
          should be printed.

fragment: Logical indicating whether file represents a complete Rd
          file, or a fragment.

warningCalls: Logical: should parser warnings include the call?

  macros: Filename or environment from which to load additional macros,
          or a logical value.  See the Details below.

permissive: Logical indicating that unrecognized macros should be
          treated as text with no warning.

       x: An object of class Rd.

 deparse: If 'TRUE', attempt to reinstate the escape characters so that
          the resulting characters will parse to the same object.

     ...: Further arguments to be passed to or from other methods.

_D_e_t_a_i_l_s:

     This function parses 'Rd' files according to the specification
     given in <URL: https://developer.r-project.org/parseRd.pdf>.

     It generates a warning for each parse error and attempts to
     continue parsing.  In order to continue, it is generally necessary
     to drop some parts of the file, so such warnings should not be
     ignored.

     Files without a marked encoding are by default assumed to be in
     the native encoding.  An alternate default can be set using the
     'encoding' argument.  All text in files is translated to the UTF-8
     encoding in the parsed object.

     As from R version 3.2.0, User-defined macros may be given in a
     separate file using '\newcommand' or '\renewcommand'.  An
     environment may also be given: it would be produced by
     'loadRdMacros', 'loadPkgRdMacros', or by a previous call to
     'parse_Rd'.  If a logical value is given, only the default
     built-in macros will be used; 'FALSE' indicates that no '"macros"'
     attribute will be returned with the result.

     The 'permissive' argument allows text to be parsed that is not
     completely in Rd format.  Typically it would be LaTeX code, used
     in an Rd fragment, e.g. in a 'bibentry'.  With 'permissive =
     TRUE', this will be passed through as plain text.  Since
     'parse_Rd' doesn't know how many arguments belong in LaTeX macros,
     it will guess based on the presence of braces after the macro;
     this is not infallible.

_V_a_l_u_e:

     'parse_Rd' returns an object of class '"Rd"'.  The internal format
     of this object is subject to change.  The 'as.character()' and
     'print()' methods defined for the class return character vectors
     and print them, respectively.

     Unless 'macros = FALSE', the object will have an attribute named
     '"macros"', which is an environment containing the macros defined
     in 'file', in a format that can be used for further 'parse_Rd'
     calls in the same session.  It is not guaranteed to work if saved
     to a file and reloaded in a different session.

_A_u_t_h_o_r(_s):

     Duncan Murdoch

_R_e_f_e_r_e_n_c_e_s:

     <URL: https://developer.r-project.org/parseRd.pdf>

_S_e_e _A_l_s_o:

     'Rd2HTML' for the converters that use the output of 'parse_Rd()'.

