\documentclass{article}
%include polycode.fmt
\usepackage{hyperref}
\begin{document}
\setcounter{secnumdepth}{5} %allow paragraphs to be numbered. I.e. depth of four === subsubsubsection
\section{The Way of the Lens}

A look at this \href{http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html}{blog post}.

\subsection{What you need to know?} 
TODO: We also need to explain single and multi-parameter lambda expressions. Type vs value-constructors. 

\subsubsection{Simple Records}
Warning: See this \href{http://stackoverflow.com/questions/4260507/avoiding-namespace-pollution-in-haskell}{field-name namespace pollution}. 
Also (more up-to-date) \href{http://stackoverflow.com/questions/24352280/haskell-multiple-declarations-of-x}{look here}, which says 2016 Haskell will include a language
extension to fix this issue, and the curren solution is to prefix field names.
For further reading check the links from the accepted answer here: \href{http://stackoverflow.com/questions/19176645/current-best-practice-regarding-record-global-namespacing}{read this}.
I will use the simple 'prefix' solution for now.

The User type introduced below makes use of the Haskell record-syntax. This means creating a value of type User can be done in one of two ways. The first
we shall call 'traditional syntax' and the second 'field name syntax':
\begin{code}
module Lenses where
data User = User {name :: String, age :: Int} deriving (Show)

user1 = User "Superman" 1000
user2 = User {name = "Batman", age = 40}
superHero = User {name = "DandyAss", age = 17}
\end{code}

\subsubsection{Complex Records}
\begin{code}
data Program = Program {progName :: String, account :: Account} deriving (Show)
data Account = Account {accNumber :: Int, owner :: Member} deriving (Show)
data Member = Member {fName :: String, lName :: String, mAge :: Int} deriving (Show)

program = Program "Nectar" (Account 123 (Member "Donald" "Rump" 70))
\end{code}


We start as simply as possible by looking at 'getters' and 'setters' on a simple record type.

\subsection{Way of Naked Haskell}
\subsubsection{Simple Records}
An important point is that if any type defines its values using record-syntax, then for each field the compiler generates getters and setters with the same name as the field.
\begin{code}
-- compiler generated getter, name :: User -> String.
currentName = name superHero
-- compiler generated setter. Returns a clone record but with the specified field changed.
improvedSuperHero = superHero {name = "X-Tron"}
\end{code}
The |currentName| and |improvedSuperHero| are the Naked Way. But there is a problem. The main problem with the above only becomes apparent when you look at complex records.
It turned out people came up with increasingly general solutions which eventually resulted in a general construct with wide ranging properties.
\subsubsection{Complex Records}
How do we get and set values on a complex record? Look here:
\begin{code}
-- get the program-account's number.
accountNumber = accNumber (account program)
-- get the program-account-member's fName
firstName = fName (owner (account program))
\end{code}
Compared to the Java world (program.account.owner.fName), this is a little odd to read (it's backwards!).
...Now show the setters at play...TBD
\begin{code}
-- change the program-account number
newProgram = Program (progName program) ((account program) {accNumber = 321})
-- change the program-account-member's fName
changedTheOwnerFName = program {account = (account program) {owner = (owner (account program)) {fName = "Ronald"}}}
-- Or , without record syntax
changedTheOwnerFName1 = Program (progName program) (Account (accNumber (account program)) (owner (account program)) {fName = "Ronald"})
\end{code}
It takes a few minutes to construct horrors like these with all the right brackets in-place!

\subsubsection{Definition of the problem}

\subsubsection{Way of Naive Lens}
Next we define a type-template whose value definition defines two function-signature templates.
You create an actual value by supplying two functions: NaiveLens {(\o -> body) (\v o -> body)}
\begin{code}
data NaiveLens o v = NaiveLens
                         { get :: o -> v
                         , set  :: v -> o -> o }
\end{code}

Here's an example which uses traditional value-constructor syntax (i.e. no named fields):
\small
\begin{code}
nameLens :: NaiveLens User String
nameLens = NaiveLens (\user -> name user) (\str user -> user {name = str})
\end{code}
\normalsize

How would we use the nameLens? First, remember nameLens is a type with two fields named 'get' and 'set'. Also remember the compiler will have generated functions allowing
those fields to be manipulated also called 'get' and 'set'.
So | get nameLens | uses the compiler generated function to return the first field (the getter function), while | set nameLens | returns the second field (setter function).
Second, given these expressions return functions, they must therefore be supplied with the appropriate arguments. Let's see how they're used.
\subsubsection{Simple Records}
\begin{code}
superHeroName = (get nameLens) superHero
superHeroTwin = (set nameLens) "Defunct" superHero
\end{code}
Here |(get nameLens)| returns the first function defined in the nameLens record i.e. |(\user -> name user) :: User -> String |. Given the User record 'john', it uses the compiler 
generated function to get the value of the 'name' field | name user |.
Likewise, |(set nameLens)| returns the second function of the nameLens record i.e. |(\str user -> user {name = str}) :: String -> User -> User|. 
Given a new value "Junkie" and the User record 'john', it creates a clone of 'john' but with the name now changed to "Junkie".
This is beginning to look more digestible. Rather like 'object.getFieldName()' in Java, we have '(get field-name) record' in our Lens world.
\subsubsection{Complex Records}
Let's go right for the guts of the record and set-up the Lenses to manipulate "program-account-member's fName".
First, here's the definition of our records for reference:
\begin{spec}
data Program = Program {progName :: String, account :: Account} deriving (Show)
data Account = Account {accNumber :: Int, owner :: Member} deriving (Show)
data Member = Member {fName :: String, lName :: String, mAge :: Int} deriving (Show)
\end{spec}
First let's set up our Lenses:
\begin{code}
memberFNameLens :: NaiveLens Member String
memberFNameLens = NaiveLens fName (\newName member -> member {fName = newName})
accountOwnerLens :: NaiveLens Account Member
accountOwnerLens = NaiveLens owner (\newMember account -> account {owner = newMember})
programAccountLens :: NaiveLens Program Account
programAccountLens = NaiveLens account (\newAccount program -> program {account = newAccount})
\end{code}
Before we continue, let's quickly look at how we change the fName
\begin{code}
mandy = set memberFNameLens "Mandy" (get accountOwnerLens (get programAccountLens program))
\end{code}
Already, it's looking more readable. But we're going to take a quick aside and look at a powerful tool from functional programming - function composition.
Essentially, we're going to see how easily we can combine primitive Lenses to form complex Lenses.
First, look at the following type-clues:
\begin{spec}
get accountOwnerLens :: Account -> Member
get memberFNameLens :: Member -> String
get memberFNameLens (get accountOwnerLens) :: Account -> String
\end{spec}
But this looks like the first field in a getter function which skips from an 'Account' to its 'Member' 'FName' field:
|accountOwnerFNameLens :: NaiveLens Account String = NaiveLens {(get memberFNameLens (get accountOwnerLens), ...setter...}|.
Given, |f . g = \x -> f (g x)|, we can write the composition as |(get memberFNameLens . get accountOwnerLens), ...|.

What about 'setters'? Look at the following setter composition which we break-down using helper functions because it's more intricate:
\begin{code}
--1 set a new Member on an Account
setAccountMember :: Member -> Account -> Account
setAccountMember m a = (\member acct -> acct {owner = member}) m a
--2 Set a new fName on a Member
setMemberFName :: String -> Member -> Member
setMemberFName newName m = (\str member -> member {fName = str}) newName m
--3 can we compose the above together?
setAccountMemberFName :: String -> Account -> Account
setAccountMemberFName newName acc = (setAccountMember (setMemberFName newName (owner acc)) acc)
\end{code}
Out of interest, this is the expanded lambda expression:
\begin{spec}
setAccountMemberFName2 :: String -> Account -> Account
setAccountMemberFName2 newName acc = (\member acct -> acct {owner = member}) ((\str member -> member {fName = str}) newName (owner acc)) acc
\end{spec}
Notice this is not expressible using function composition syntax because |(owner acc)| is dependent upon acc from the outer scope.
THE ANSWER IS currently IN ScratchPad.lhs.


\subsection{Ex: Write an ageLens}
Answer:
\begin{code}
ageLens :: NaiveLens User Int
ageLens =  NaiveLens age (\newAge user -> user {age = newAge})
\end{code}
\subsection{Write code that decrements 'age' by ten}
Now let's remind ourselves of how to 'get' and how to 'set' a User's age:
\begin{code}
currentAge = get ageLens superHero
warpedSuperHero = set ageLens 79 superHero
\end{code}
So let's combine the above functions so we can produce a new User who looks 10 years younger:
\begin{code}
rejuvenatedSuperHero = set ageLens ((get ageLens superHero) - 10) superHero
\end{code}

This is getting clunky - especially to achive so little. We are forced to 'get' the value, transform it, then set the new value on a new record.
So let's ask, what are we trying to achieve here? We want to transform/manipulate the value that is already set on the existing record...to become the new value set on the new record.
Thinking non-functionally, we want to say something like 'newRecord.newValue = fn(oldRecord.oldValue), where 'tfn' is some tranformation function. 
Now, looking at the ageLens 'setter' we see the expression |{age = newAge|. We would really like this to be |{age = tfn age}| 
(remember, the 'age' on the left is actually identifying the field belonging to the new record).

As good Haskellians we first ask 'what is the type of tfn?'. In all cases, given tfn transforms values between the same field on different records, so
it must be FieldType -> FieldType.
Also, we'd like tfn to be as flexible as possible and thus passed in as a parameter during the 'set' operation. We therefore change our 'set' type to allow tfn to be passed: 
\begin{spec}
\tfn user -> user {age = tfn (age user)} :: (Int -> Int) -> User -> User
\end{spec} 
Notice: newAge parameter is gone, it's defunct as we now calculate the new age on the fly using 'tfn'.
This function works by passing in a function 'tfn' that transforms Int -> Int. We then get the current age of the current User |(age user)|. We then tranform that age |fn (age user)|.
Finally, we proceed as before.
So let's rewrite our Lens (NOTE: It appears having two 'get' methods of the same type causes a name clash 'Multiple declarations of get' see
\href{https://bloggablea.wordpress.com/2007/04/24/haskell-records-considered-grungy/}{this blog post}):
\small
\begin{code}
data ImprovedLens o v = ImprovedLens
                         { getImproved :: o -> v
                         , setImproved  :: (v -> v) -> o -> o }
                         
ageLens2 :: ImprovedLens User Int
ageLens2 = ImprovedLens (\user -> age user) (\tfn user -> user {age = tfn (age user)})

currentAge2 = getImproved ageLens2 superHero
oapSuperHero = setImproved ageLens2 (\age -> 3 * age) superHero
\end{code}
\normalsize
This looks a lot easier than the version above, we can update a field without the ugly get, and it composes naturally.

\subsection{Further Issues}
OK, so we've introduced the 'transformation function' and it's given us a couple of cool wins. But if we look more closely it introduces a new problem.
The first question is, what if that function can fail? This is modelled in Haskell using the Maybe functor:
\begin{spec}
data Maybe a = Just a | Nothing
\end{spec}

So a function which might fail is written |f :: a -> Maybe a| to show the return might be |Just a| or |Nothing| if it fails. Now let's trace the failure circuit through
our 'setter'. If the function is applied to 'age' of 'Member' and it fails, then the object we were going to return should also be marked as failed.
So our signature for 'setter' should be: |setFailing :: (v -> Maybe v) -> o -> Maybe o|.

In theory, we would now have to write (following SPJ on SkillsMatter, collecting everything we've learned, using 'mod' for setter which modifies the value):
\begin{spec}
data Lens o v = Lens 
                        {   get :: o -> v,
                            set :: v -> o -> o,
                            mod :: (v -> v) -> o -> o,
                            modM :: (v -> Maybe v) -> o -> Maybe o }                                    
\end{spec}
 It gets worse when we ask 'what about return values that model side-effects?':
\begin{spec}
data Lens o v = Lens 
                        {   get :: o -> v,
                            set :: v -> o -> o,
                            mod :: (v -> v) -> o -> o,
                            modM :: (v -> Maybe v) -> o -> Maybe o,
                            modIO :: (v -> IO a) -> o -> IO a }                                    
\end{spec}
set :: Functor f => (v -> f v) -> o -> f o
\end{document}

