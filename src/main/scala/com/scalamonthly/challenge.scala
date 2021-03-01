package com.scalamonthly

import cats.data.NonEmptyList

object challenge {

  final case class EmployeeInfo(name: String, salesRevenue: Int, salary: Int)
  sealed abstract class Employee extends Product with Serializable{
    def fold[A](ic: EmployeeInfo => A)(compute: (EmployeeInfo, List[A]) => A):A = this match {
      case Employee.Manager(info, directReports) => compute(info, directReports.map(_.fold(ic)(compute)))
      case Employee.IndividualContributor(info) => ic(info)
    }
  }
  object Employee {
    final case class Manager(info: EmployeeInfo, directReports: List[Employee]) extends Employee
    final case class IndividualContributor(info: EmployeeInfo) extends Employee
  }
  final case class BranchName(value: String) extends AnyVal
  final case class Branch(name: BranchName, manager: Employee.Manager)
  final case class BranchSummary(name: BranchName, totalRevenue: Double, totalSalary : Double, employeeCount: Int)

  def toBranchSummary(branch: Branch): BranchSummary = {

    def totalRevManager(manager: Employee.Manager):Int = manager.fold(info => info.salesRevenue)((info, revList) => info.salesRevenue + revList.fold(0)(_+_))

    def totalSalary(manager: Employee.Manager): Int = manager.fold(info => info.salary)((info, salaryList) => info.salary + salaryList.sum)
    def totalCount(manager: Employee.Manager): Int = manager.fold(_ => 1)((_, empCount) => empCount.sum + 1)

    BranchSummary(branch.name, totalRevManager(branch.manager), totalSalary(branch.manager), totalCount(branch.manager))
  }

  def determineBranchToShutDown(branches: NonEmptyList[Branch]): BranchName = branches.map(toBranchSummary).foldLeft(toBranchSummary(branches.head))((minBranch, branchSummary) => if(profitPerEmployee(minBranch) < profitPerEmployee(branchSummary)) minBranch else branchSummary).name

  def profitPerEmployee(summary: BranchSummary): Double = (summary.totalRevenue - summary.totalSalary)/ summary.employeeCount
  /*determine the profit per employee of the branch and output the branch which makes the least amount of profit-per-employee so it can be shut down.*/

}