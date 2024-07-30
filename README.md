# Eugen: research project generator

## What is Eugen?
A generator of research project plan. It allows you to generate a full plan from a compact, not redundant description. 

I wrote it one day that I got sick of chasing references and updating efforts every time a small update was done. 

The name is from "EU GENerator" since in its current form it outputs project description in a way compatible with EU research project, but it should be flexible enough to be used even in other contexts. 

## Motivation (Why did you write it?)

If you had any experience with writing research projects, certainly you noticed that the project description is quite redundant.  For example, 
* For every work-package (WP) you need to write the effort (in person month -- PM) of every partner and, moreover, summarize the efforts in a table work-package vs. partner, with the total effort per partner and per WP.
* For every WP you must describe the deliverables of the WP, but also summarize all the deliverables in a table (possibly ordered by date). 
* Every WP is made of task and you have dependences between tasks (e.g., a specific task can begin only after another ends) and between tasks and deliverables.
* Everything needs to be summarized in a GANTT chart

If you increase, say, the duration of a given task you can experience a ripple of changes since dependent tasks will begin later, effort in PM can change, deliverables produced by the task will appear later, corresponding milestones will change, the GANTT will change too...  Forgetting something is just too easy and, given the highly competitive nature of EU funding, any mismatch will be a death sentence for your project.

Personally, I _hate_ (to be read with the voice of [Grouchy Smurf](https://smurfs.fandom.com/wiki/Grouchy_Smurf) :smile:) this kind of "walking on eggs" uncertainty (why do you think I program in Ada?). Moreover, I _hate_ repetitive tasks and one day I got sick and wrote this fairly big piece of code (the content of `src/` directory is slight more than 1 Mbyte).

I recently used it to write a EU research proposal and it served me well. Although the research proposal is being reviewed (so I do not know if it will get funded or not),  it helped me **a lot**.

## Conceptual model

The model of project used in this software is very classical
* The project involves one or more _partners_
* The work is organized in _work packages_ (WPs), every WP has one or more _tasks_
* Every WP has _deliverables_
* A deliverable is produced by one or more tasks
* The project has _milestones_, some milestone can be associated to a deliverable
* WP and tasks have
  * a begin date, an end date, a duration
  * a description
  * a name and an optional short name (that can be used in contexts where space is scarce)
  * a label (for cross-reference)
  * effort in person-month (PM) for every partner
* deliverables and milestones have
  * An expected date (a deliverable can have several expected dates, e.g., for periodic reports)
  * A description 
  * a name and an optional short name (that can be used in contexts where space is scarce)
  * a label (for cross-reference)
  
## How do I use it?

In order to explain how to use the program it is worth describing briefly its internal structure.  The program internally has three internal stages

```mermaid
graph LR
    Description --> Parser --> I[Internal form] --> Processor --> O[Output files]
```

* The frontend of the program is a *Parser* that reads an external description of the project and converts it in an *internal format*
* The internal format is handed to a *Processor* that outputs one or more files with the project description. Currently several LaTeX files with WP description, deliverable list, GANTT, ... are produced

Both *parser* and *processor* are *plugin-based*.  This means that parsers and processors are internally represented as tagged types implementing  a specific *interface*.  This allows one to easily add new input and output formats (of course, adding new plugins requires writing Ada code).


