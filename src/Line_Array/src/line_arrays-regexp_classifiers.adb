pragma Ada_2012;
package body Line_Arrays.Regexp_Classifiers is

   ---------
   -- Add --
   ---------

   procedure Add (To       : in out Classifier_Type;
                  Regexp   : String;
                  Callback : Callback_Type)
   is
      Matcher : constant Gnat.Regpat.Pattern_Matcher := Gnat.Regpat.Compile (Regexp);
   begin
      To.Exprs.Append (New_Item => Regexp_Descriptor'(Size     => Matcher.Size,
                                                      Matcher  => Matcher,
                                                      Callback => Callback));
   end Add;

   -----------------
   -- Add_Default --
   -----------------

   procedure Add_Default
         (To : in out Classifier_Type; Callback : Callback_Type)
   is
   begin
      if To.Default /= null then
         raise Double_Default;
      end if;

      To.Default := Callback;
   end Add_Default;

   ------------
   -- Create --
   ------------

   function Create (Regexps : Regexp_Array) return Classifier_Type
   is
      Result : Classifier_Type;
   begin
      for R of Regexps loop
         if Is_Default (R) then
            Result.Add_Default (R.Callback);
         else
            Result.Add (Regexp   => To_String (R.Regexp),
                        Callback => R.Callback);
         end if;
      end loop;

      return Result;
   end Create;

   --------------
   -- Classify --
   --------------

   function Classify
         (Classifier : Classifier_Type; Line : String) return Classified_Line
   is
      use Gnat.Regpat;
   begin
      for Regexp of Classifier.Exprs loop
         declare
            Matched : Match_Array (0 .. Paren_Count (Regexp.Matcher));
         begin
            Gnat.Regpat.Match (Self    => Regexp.Matcher,
                               Data    => Line,
                               Matches => Matched);

            if Matched (0) /= No_Match then
               return Regexp.Callback (Line, Matched);
            end if;
         end;
      end loop;

      if Classifier.Default = null then
         raise Constraint_Error;
      else
         declare
            Matched : constant Match_Array (0 .. 0) :=
                        (0 => Match_Location'(First => Line'First,
                                              Last  => Line'Last));
         begin
            return Classifier.Default (Line, Matched);
         end;
      end if;
   end Classify;

end Line_Arrays.Regexp_Classifiers;
