* Facebook 
    * How do you when a friend likes what you like. 
    * How would you keep up-to-date a list of which friends are online. 
    * Design facebook newsfeed. 
    * Design Facebook Chat 
* Google 
    * How do you store search history? 
    * How would you make an autocomplete? 
    * How to design docs? Without multiple users, with? 
* YouTube 
    * How do you design a system to track likes/dislikes. 
* Bitly 
    * Design Bitly. 
* LinkedIn 
    * How to calculate number of hops between two people. 
* Paypal: design PayPal. How to maintain consistency? 
* Twitter 
    * Design trending topics. 
    * Design feeds 
* Elevator: Design an elevator 
* Event logging 
    * Ingest events; want to be able to ask how many of a given event in the last 60sec. 
* Airbnb 
    * Design system to answer boolean queries of options across listings. 
* Games 
    * An online chess playing service 
* Amazon: Design a shopping cart 
* OpenTable: Design reservation system.



I also have a writeup in tech-notes about how I’d try to implement Facebook.
Serialize operations in any order. Store the linear history.
This will eventually become consistent, but may display data which could change underneath you.
To avoid joins, you can denormalize: for instance, you can fold friend’s profiles into your friendships table.
Problem: now you have to have a plan for keeping that data up-to-date.
* Post
    * # of likes for each category; first couple of people who reacted (btw, just your friends).
        * Then you can query for more people who reacted.
        * Prolly can fake this by doing a join on the client with cached info you have!
        * Prolly stores *why* you were shown an item along with that item in your feed.
            * That way, even if you don’t download all likes, you can still see your friends who liked the thing.
    * First couple of comments
        * Then you can click through to get more easily
        * Not all comments are sent, though. Just the most recent ones maybe?
    * Each like, post, or comment needs a user’s image.
        * You can always use cached versions for friends.
        * For people you don’t have in the client’s cache, you can have denormalize and store an old version.
        * It’s probably desirable to refresh that every so often.
        * This way, posts that people don’t look at don’t cost you time being refreshed.
        * Prolly can put a fairly long cache time on this since these commenters don’t matter to your user.
* Edits? Deletions?
    * As mentioned, user data is largely cacheable.
    * If I edit the text of a comment?
        * Only needs to be edited in one place in the DB.
        * But should edit the post also.
        * I guess when the comment gets fetched again this will be taken care of?
    * Or edit a post.
        * Needs to affect all feeds?
    * Deletion of a post or comment
        * That *definitely* needs to be reflected many places.
* Feed
    * So your users have streams of activity. Actually, any item has a stream of activity.
    * To assemble a feed, you look at the things you are following.
    * Then you pull the latest activities.
