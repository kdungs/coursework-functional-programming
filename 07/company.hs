data Person = Person {
    name :: String,
    salary :: Float
} deriving (Eq, Ord, Show)

data Department = Department {
    manager :: Person,
    employees :: [Person]
} deriving Show

type Company = [Department]


example :: Company
example = [Department {
    manager = Person {name = "Petersen, Peter", salary = 1e5},
    employees = [
        Person {name = "Hansen, Hans", salary = 3e3},
        Person {name = "Klausen, Klaus", salary = 3.1e2}
    ]
}]

doubleSalary :: Person -> Person
doubleSalary Person {name=n, salary=s} = Person {name = n, salary = 2 * s}

doubleDepartmentSalaries :: Department -> Department
doubleDepartmentSalaries Department {manager=m, employees=es} = Department {
    manager = doubleSalary m,
    employees = map doubleSalary es
}

doubleCompanySalaries :: Company -> Company
doubleCompanySalaries [] = []
doubleCompanySalaries (d:ds) = (doubleDepartmentSalaries d):
    (doubleCompanySalaries ds)

main = do
    print $ example
    print $ doubleCompanySalaries example
