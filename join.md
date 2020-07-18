## Join based queries with skunk decoders - folding with a Semigroup

### Domain Model

Let's take a simple example that models an employee along with the salaries accrued in a year. Here's the domain model in Scala:

```scala
case class Salary(empId: String, month: Int, amount: BigDecimal)
case class Employee(id: String, name: String, salaries: List[Salary])
```

### Data Model

We implement the data model with a join on 2 tables - `employees` and `salaries`:

```sql
CREATE TABLE IF NOT EXISTS employees (
  id varchar NOT NULL PRIMARY KEY,
  name varchar NOT NULL
);
 
CREATE TABLE IF NOT EXISTS salaries (
  id serial PRIMARY KEY,
  empId varchar NOT NULL references employees(id),
  month smallint NOT NULL,
  amount decimal NOT NULL
);
```

Clearly to build an `Employee` from the above 2 tables in the database we would need to do a join and then combine all joined records together for one employee to collect the salaries and make the composition a `List[Salary]`.

### Decoders in skunk

Here's how we would build the query using decoders in skunk:

```scala
private object EmployeeQueries {
  val empSalaryDecoder = varchar ~ int4 ~ numeric ~ varchar
  val selectAll =
    sql"""
        SELECT e.name,
               s.month,
               s.amount,
               e.id
        FROM employees e, salaries s
        WHERE e.id = s.empId
      """.query(empSalaryDecoder)
  }
``` 

### User API for the Query in Repository

Now we can use the above machinery to implement the final user API for selecting all employees from the database:
 
```scala
final class EmployeeRepository[M[_]: Sync] (
  sessionPool: Resource[M, Session[M]]
) {
    import EmployeeQueries._

    def query: M[List[Employee]] = {
      sessionPool.use { session =>
        session
          .execute(selectAll)
          .map(_.groupBy(_._2))
          .map { m =>
            m.map {
              case (empId, list) =>
                val employeeSalaryJoinRecords =
                  makeSingleEmployeeSalary(empId, list)
                employeeSalaryJoinRecords
                  .tail
                  .foldLeft(employeeSalaryJoinRecords.head)(
                    Semigroup[Employee].combine
                   )
            }.toList
          }
        }
    }

    private def makeSingleEmployeeSalary(
    
      empId: String,
      empSalaries: List[String ~ Int ~ BigDecimal ~ String]
      
    ): List[Employee] = {
      empSalaries.map {
        case ename ~ month ~ amount ~ eid =>
          Employee(eid, ename, List(Salary(eid, month, amount)))
      }
    }
}
``` 

Note what happens in the above implementation:

* we execute the database query using the `selectAll` combinator that we defined earlier
* we then group the records based on employee id. Note this is the reason we picked up `id` as the last column in the select statement - in that case we can extract `id` from the twiddle list, which is left-associated chain of pairs. We could not have extracted the `id` from the list of pairs easily had we selected it as the first column where it naturally occurs in the `employees` table.
* now we have a `Map` which we then iterate over key value pairs - the key being the `id` and the value being a single joined record with the layout as described in the decoder.
* the actual trick is to fold over these records using a `Semigroup[Employee]`, which needs to be defined suitably so that salaries belonging to one employee get collected in a list with that specific employee. Here's how we can do that:

```scala
object Employee {
  implicit val employeeConcatSemigroup: Semigroup[Employee] = new Semigroup[Employee] {
    def combine(x: Employee, y: Employee): Employee =
      x.copy(salaries = x.salaries ++ y.salaries)
  }
}
```