# What is Eugen?
A generator of research project plan. It allows you to generate a full plan from a compact, not redundant description. 

I wrote it one day that I got sick of chasing refernces and updating efforts every time a small update was done.  

The name is from "EU GENerator" since in its current form it outputs project description in a way compatible with EU research project, but it should be flexible enough to be used even in other contexts. 

# Motivation (Why did you write it?)

If you had any experience with writing research projects, certanly you noticed that the project description is quite redundant.  For example, 
* For every workpackage (WP) you need to write the effort (in person month -- PM) of every partner and, moreover, summarize the efforts in a table workpackage vs. partner, with the total effort per partner and per WP.  
* For every WP you must describe the delivarables of the WP, but also summarize all the deliverables in a table (possibly ordered by date).  
* Every WP is made of task and you have dependences between tasks (e.g., a specific task can begin only after another ends) and between tasks and deliverables.
* Everything needs to be summarized in a GANTT chart

If you increase, say, the duration of a given task you can experience a ripple of changes since dependent tasks will begin later, effort in PM can change, deliverables produced by the task will appear later, corresponding milestones will change, the GANTT will change too...  Forgetting something is just too easy and, given the highly competitive nature of EU funding, any mismatch will be a death sentence for your project.

Personally, I _hate_ (to be read with the voice of [Grouchy Smurf](https://smurfs.fandom.com/wiki/Grouchy_Smurf) :smile:) this kind of "walking on eggs" uncertainity (why do you think I program in Ada?)