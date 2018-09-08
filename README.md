# Book-Recommender
Recommender Engine for the Books on GoodReads Dataset: deeper insight at data-driven book recommendations

Collaborative filtering is a standard method for product recommendations. To get the general idea consider this example:

Imagine you want to read a new book, but you don’t know which one might be worth reading. You have a certain friend, with whom you have talked about some books and you typically have had quite a similar opinion on those books. It would then be a good idea to ask this friend whether he read and liked some books that you don’t know yet. These would be good candidates for your next book.
 It works as follows:

You first identify other users similar to the current user in terms of their ratings on the same set of books.
For example, if you liked all the “Lord of the rings” books, you identify users which also liked those books.
2. If you found those similar users you take their average rating of books the current user has not yet read …

So, how did those “Lord of the rings” lovers rate other books? Maybe they rated “The Hobbit” very high.

… and recommend those books with the highest average rating to him.
Accordingly, “The Hobbit” has a high average rating and might be recommended to you.

These three steps can easily be translated into an alogrithm
