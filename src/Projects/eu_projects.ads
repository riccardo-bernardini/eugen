with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Regexp_Readers.Generic_Readers;

--
-- This package hierarchy provides resources to operate with the description
-- of research projects (mostly EU-style, but it can be used in other
-- contexts).
--
-- The model for a project is the following
--
--     * A project has
--         * A name
--         * A short name
--         * A sequence of partners
--         * A sequence of WPs
--         * A risk lists
--
--     * Every PARTNER has
--         * Name, short name, ID, index
--         * A description
--         * A set of roles, every role hase
--             * name, ID
--             * descripiton
--             * cost
--
--     * Every WP has
--         * Name, short name, ID, index, description
--         * WP type (research, admin, ...)
--         * A sequence of tasks
--         * Computed attributes:
--             - begin/end dates
--             - partner effort
--             - deliverables
--             - milestones
--
--     * Every task has
--          * Name, short name, ID, index, description
--          * A parent WP
--          * A dependence list (task depends on...)
--          * A sequence of activity periods.  Every period has
--              - begin/end date
--              - intensity
--              - partner effort, every effort has
--                  + partner ID
--                  + role
--                  + number of PM
--          * A set of milestones
--          * A set of deliverables
--
--     * Every milestone has
--          * Name, short name, description, ID, index
--          * A parent task
--          * A due date
--
--     * Every deliverable has
--          * Name, short name, description, ID, index
--          * A parent task
--          * A due date
--
-- A few comments about the data involved
--
--     * Name and short name: they are used to be included in the
--       research proposal by using a special syntax (#{...} ruby-like or
--       \R{...} LaTeX-like?) that uses as parameter entity attributes
--       optionally decorated
--
--     * The dates can be in two format: symbolic and absolute.  The idea
--       is that we want to allow for dates like "design.start + 3" to denote
--       three months after the end of the design WP.  However, when
--       we need to create, say, a GANNT we need actual months that can be
--       obtained by computing all the symbolic dates.
--
--     * For every element we have at least two indexes: a "local" and
--       a "full" one.  For example, task T4.3 has "3" as a local index
--       and "4.3" is the global one and "T4.3" is the "decorated" one.
--       While local and full are not necessarily unique (4.3 can also
--       be a deliverable or a milestone of WP4), the decorated is unique.
--
-- IDs are "labels" for entities and have a hierarchical structure, like
--
--                  wp1.task2        -- A task of WP1
--                  wp1.blue_print   -- A deliverable of WP1
--
-- In the context of WP1 only task2 or blue_print are necessary. We will
-- call the parts separated by "." basic IDs.
--
-- Basic IDs are case insensitive and have the usual "Ada identifier syntax"
-- (letters, numbers and underscores, begin with a letter, no double or
-- final underscore allowed).  We cannot use '-' (as it is allowed in XML)
-- since we could want to use the IDs in expression and ambiguity
-- could arise with subtraction.
--
-- An attribute is an ID with a final ".attribute", for example
--
--         wp1.task2.begin   -- The begin date of task2
--         wp1.duration      -- The overall duration  of WP1
--
-- Date attributes can be used in expressions.  Attributes can also be included
-- in descriptions using a syntax like
--
--           #command{wp1.task2.begin?option1=a, option2=b}
--
--  where "command" is a specific command and it can be empty
--  (that is, something like "#{wp1.name}" is allowed). The part after "?"
--  is optional and it is used for fine control the attribute format;
--  for example,
--
--           #{dissemination.name?short}
--
--  could be used to insert the "short name" of WP dissemination.
--  We could consider the possibility of default attributes and options
--  so that something like
--
--           #{dissemination}
--
--  is possible.  The idea for the default is the following
--
--      * Every "class" of entity (WP, task, deliverable, ...) has a default
--        attribute.
--      * Every attribute associated with an entity (e.g., name of a WP)
--        has a default option list
--      * The maps
--
--            class              -> attribute
--            (class, attribute) -> option
--
--        are part of the project attributes.
-- --------------------------------------------------------------------
-- ** Parsers, writers, processors
--
-- In order to read/write textual representations of the projects we will
-- need parsers and writers.  Most probably initially we will support only
-- the YAML format since it is fairly easy to parse and to write by hand.
-- In order to allow for new formats we will use a "plugin-like structure"
--
-- A different matter is the production of external files with information
-- taken from the project.  For example, LaTeX sources with the WP description
-- or a GANTT chart of the activities in SVG, PDF or TeX format.
--
-- The part of code that takes care of such productions is called a
-- _processor_.  Initially most probably we will support two kinds of
-- processors
--
--     1. Template-based, much in the spirit of AWS templates.  If possible,
--        we will try to use them out-of-the-box
--     2. Server-based: an external processor is launched with some parameters
--        that include a local port that can be queried to get informations
--        about the project.  The queries have the format of an attribute
--        query (options included) with the difference that an some special
--        attributes are added, namely:
--           - tasks
--           - delivs
--           - milestones
--           - wps
--           - and maybe others
--
--        The new attributes can be used to query "all the ... of ...".  For
--        example, in order to get all the tasks of WP dissemination the
--        processor can ask for
--
--             dissemination.tasks
--
--        The answer will include a space-separated list of the ids of the
--        tasks.
--
-- ----------------------------------------------------------------
-- ** Editing projects
--
-- In editing projects we can add/remove
--     - WP
--     - tasks of a WP
--     - deliverables
--     - milestones
--     - partners
--     - roles in partners
--
-- We can also change
--     - descriptions
--     - names
--     - dates
--

--
package EU_Projects is
   -- Is a bound of 10 billions enough?
   type Currency is delta 0.01 digits 12 range 0.0 .. 9_999_999_999.99;

   Max_ID_Length : constant Positive := 256;

   --
   -- Function to check if X satisfies the syntax of IDs.  Informally, an
   -- ID has a syntax similar to an Ada identifier with the following additions
   --
   --  (1)  "." can be used inside the identifier so that "foo.bar" is OK
   --  (2)  An identifier cannot begin nor end with "_" or "."
   --  (3)  "_" must always be followed by an alphanumeric char
   --  (4)  "." must always be followed by a letter
   --  (5)  At most one "." can be present
   --
   function Is_Valid_ID (X : String) return Boolean
   is (
       (X'Length = 0)
       or else ((X'Length <= Max_ID_Length)
                and then (Is_Letter (X (X'First)) and Is_Alphanumeric (X (X'Last)))
                and then (for all I in X'Range =>
                                (Is_Alphanumeric (X (I))
                                 or else (X (I) = '_' and then Is_Alphanumeric (X (I + 1)))
                                 or else (X (I) = '.' and then Is_Letter (X (I + 1)))))
                and then (for all I in X'Range =>
                            (for all J in I + 1 .. X'Last =>
                               (if X (I) = '.' then X (J) /= '.'))))
       -- Note : in the condition above I+1 is always well defined since
       -- if X(X'Last) is alphanumeric, the last two tests are cutted
       -- away by the "or else" after  Is_Alphanumeric (X (I)), otherwise
       -- if X(X'Last) is not alphanumeric, the "for all" will not checked
       -- at all because of the preceding "and then"
      );

   package Bounded_Identifiers is
         new Ada.Strings.Bounded.Generic_Bounded_Length (Max_ID_Length);

   use Bounded_Identifiers;

   type Dotted_Identifier is new Bounded_Identifiers.Bounded_String
         with Dynamic_Predicate =>
       Is_Valid_ID (To_String (Bounded_String (Dotted_Identifier)));

   -- A simple identifier is similar to a Dotted_Identifier, but it
   -- cannot have a dot
   subtype Simple_Identifier is Dotted_Identifier
     with Dynamic_Predicate =>
       (for all Ch of To_String (Simple_Identifier) => Ch /= '.');

   function Image (X : Dotted_Identifier) return String
   is (To_String (X));

   function To_ID (X : String)return Dotted_Identifier
   is (To_Bounded_String (X))
         with Pre => Is_Valid_ID (X);

   function Verbose_To_ID (X : String) return Dotted_Identifier;
   -- Like To_ID, but it prints some debug info.  Useful for debugging.

   function Join (X, Y : Simple_Identifier) return Dotted_Identifier
   is (To_ID (To_String (X) & "." & To_String (Y)));

   function Join (X : Simple_Identifier; Y : String) return Dotted_Identifier
   is (Join (X, To_ID (Y)));

   package ID_Readers is
         new Regexp_Readers.Generic_Readers
               (Result_Type => Dotted_Identifier,
                Regexp      => Regexp_Readers.Dotted_Identifier,
                Convert     => To_ID);

   package ID_Vectors is
         new Ada.Containers.Vectors (Index_Type   => Positive,
                                     Element_Type => Dotted_Identifier);

   subtype ID_List is ID_Vectors.Vector;

   function To_ID_List (Input      : String;
                        Separators : String := " ,")
                        return ID_List;

   Bad_Identifier : exception;
   Bad_Input      : exception;

   -- The result of function 'Image associated to discrete types has
   -- a space at the beginning.  That space is quite annoying and needs
   -- to be trimmed.  This function is here so that everyone can use it

   function Chop (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Image (X : Integer) return String
   is (Chop (Integer'Image (X)));
end EU_Projects;
