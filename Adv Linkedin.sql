USE mavenfuzzyfactory;
select * from website_sessions;
select distinct
utm_content,
utm_source,
utm_campaign
from website_sessions;


select 
count(distinct w.website_session_id) as sessions,
count(distinct o.order_id) as orders,
count(distinct o.order_id)/count(distinct w.website_session_id) as conv_rate,
utm_content
from website_sessions as w
left join
orders as o
on o.website_session_id = w.website_session_id
group by 4 -- can put column number based on SELECT command 
ORDER BY 1 DESC;

SELECT 
    COUNT(DISTINCT website_session_id) AS sessions,
    utm_source,
    utm_campaign,
    http_referer
FROM
    website_sessions
WHERE
    created_at < '2012-04-12'
GROUP BY 2 , 3 , 4
ORDER BY 1 DESC;

-- Finding - GSEARCH, NON BRAND CAMPAIGN 


SELECT 
    COUNT(DISTINCT w.website_session_id) AS sessions,
     COUNT(DISTINCT o.order_id) AS orders,
     count(distinct o.order_id)/count(distinct w.website_session_id) as conv_rate
FROM
    website_sessions as w
    left join
    orders as o
    on w.website_session_id = o.website_session_id
WHERE
    w.created_at < '2012-04-12' and 
    w.utm_source = 'gsearch' and 
    w.utm_campaign = 'nonbrand';
    
-- Finding: Conversion rate less than 4% - need to reduce bids
    
SELECT *
from website_sessions
where website_session_id between 100000 and 115000;


select website_session_id,
created_at,
month(created_at),
week(created_at),
year(created_at)
from website_sessions
where website_session_id between 100000 and 215000;


select count(website_session_id)
from website_sessions;


select
year(created_at),
week(created_at),
min(date(created_at)) as week_start,
max(date(created_at)) as week_end,
count(distinct website_session_id) as sessions
from website_sessions
where website_session_id between 100000 and 115000
group by 1, 2;


select order_id,
primary_product_id,
items_purchased
from orders
order by 1;

select count(order_id)
from orders
union all
select count(distinct order_id)
from orders;

select min(order_id),
max(order_id)
from orders;

select
primary_product_id,
count(distinct case when items_purchased = 1 then order_id else null end) as single,
count(distinct case when items_purchased = 2 then order_id else null end) as two,
count(distinct order_id)
from orders
where order_id between 31000 and 32000
group by 1
union all
select
count(primary_product_id),
count(distinct case when items_purchased = 1 then order_id else null end) as single,
count(distinct case when items_purchased = 2 then order_id else null end) as two,
count(distinct order_id)
from orders
where order_id between 31000 and 32000;

select *
from website_sessions
where created_at between '2012-04-15' and '2012-05-10';


select
week(created_at),
min(date(created_at)) as start_date,
count(distinct website_session_id) as sessions
from website_sessions
where created_at < '2012-05-12' and 
utm_source = 'gsearch'and
utm_campaign = 'nonbrand'
group by 1
order by 1;

-- Yes reducing bidding impacted the volume of sessions to drop

select
w.device_type,
count(distinct w.website_session_id) as sessions,
count(distinct o.order_id) as orders,
count(distinct o.order_id)/count(distinct w.website_session_id) as conv_rate
from 
website_sessions as w 
left join
orders as o 
on w.website_session_id = o.website_session_id
where w.created_at < '2012-05-11' and 
w.utm_source = 'gsearch'and
w.utm_campaign = 'nonbrand'
group by 1;

-- Findings - conversion rate for desktop is 3.7% and for mobile is less than a 1%


select
week(created_at) as week,
min(date(created_at)) as start_date,
count(distinct case when device_type = 'desktop' then website_session_id else null end) as desktop,
count(distinct case when device_type = 'mobile' then website_session_id else null end) as mobile,
count(distinct website_session_id)
from
website_sessions
where created_at between '2012-04-15' and '2012-06-09' and 
utm_source = 'gsearch' and utm_campaign = 'nonbrand'
group by 1;

-- see an impact on desktop sessions post week 20 - they have gone up as a result of bid ups
-- Important to analyse most-viewed and first-visited (entry) pages

-- temporary tables are for current sessions and if you open the new session you will have to run the query again

select *
from website_pageviews;

select 
count(distinct website_pageview_id)
from website_pageviews; 

select pageview_url,
count(distinct website_pageview_id)
from website_pageviews
where website_pageview_id < 1000
group by 1
order by 2 DESC;

select
count(distinct website_pageview_id)
from website_pageviews
where website_pageview_id < 1000;

-- How to get to the most-entred pages
-- first pageview_url that is the first page-view id for a given website session id

create temporary table first_pageview
select
website_session_id,
MIN(website_pageview_id) as min_pageview_id
from website_pageviews
where website_pageview_id < 1000
group by 1;

select *
from first_pageview;

select
count(distinct website_session_id)
from first_pageview;

select
w.pageview_url as landingpage,
count(distinct f.website_session_id) as session_hitting_lp
from first_pageview as f
left join website_pageviews as w 
on f.min_pageview_id = w.website_pageview_id
group by 1;

-- above query results in - that among 1000 pageviews - 523 sessions landed on homepage

-- most-viewed website pages ranked by session before june 9, 2012

select
pageview_url,
count(distinct website_pageview_id) as session_count
from website_pageviews
where created_at < '2012-06-09'
group by 1
order by 2 DESC;

-- top entry pages ranked by volume

select * from website_pageviews
where created_at < '2012-06-12';

select website_session_id,
min(website_pageview_id) as landing_page
from website_pageviews
where created_at < '2012-06-12'
group by 1;

create temporary table Assg_Top_entry
select website_session_id,
min(website_pageview_id) as landing_page
from website_pageviews
where created_at < '2012-06-12'
group by 1;

select * from Assg_Top_entry;

select
b.pageview_url as landing_pageurl,
count(distinct a.website_session_id) as session_count
from assg_top_entry as a left join website_pageviews as b
on a.landing_page = b.website_pageview_id
group by 1
order by 1 desc;

/* Business context: want to analyse landing page performance (by checking bounce %) for a specific time period
Step 1: find the first website_pageview_id for relevant sessions
Step 2: identify the landing page of all sessions
Step 3: counting pageviews for each sessions, to identify 'bounces' 
- if landing page had more pageviews no bounce otherwise yes bounce
Step 4: summarizing total sessions and bounced sessions, by LP */

 -- Step1

select
website_session_id,
min(website_pageview_id) as website_pageview_id
from website_pageviews
where created_at between '2014-01-01' and '2014-02-01'
group by 1;

create temporary table chap4 -- table with the session id and minimum (first entry) pageview id
select website_session_id,
min(website_pageview_id) as website_pageview_id
from website_pageviews
where created_at between '2014-01-01' and '2014-02-01'
group by 1;

select * from chap4;

-- Step 2

create temporary table chap4a -- table with the landing page url
select 
website_pageviews.pageview_url as landing_page,
chap4.website_session_id as website_session_id
from chap4 left join website_pageviews
on chap4.website_pageview_id = website_pageviews.website_pageview_id;

select * from chap4a;

select
landing_page,
count(distinct website_session_id)
from chap4a
group by 1;

-- Step 3

select chap4a.landing_page,
chap4a.website_session_id,
count(website_pageviews.website_pageview_id) as count_pageviews
from chap4a left join
website_pageviews on chap4a.website_session_id = website_pageviews.website_session_id
group by 2, 1
having count(website_pageviews.website_pageview_id) = 1;

create temporary table chap4_bouncedsessions -- table with bounced sessions only, pageview count = 1
select chap4a.landing_page,
chap4a.website_session_id,
count(website_pageviews.website_pageview_id) as count_pageviews
from chap4a left join
website_pageviews on chap4a.website_session_id = website_pageviews.website_session_id
group by 2, 1
having count(website_pageviews.website_pageview_id) = 1;


select * from chap4_bouncedsessions;

-- table with bounced session id
select chap4a.landing_page,
chap4a.website_session_id,
chap4_bouncedsessions.website_session_id as bounced_website_session_id
from chap4a left join chap4_bouncedsessions 
on chap4a.website_session_id = chap4_bouncedsessions.website_session_id
order by 2;

-- Bounce rate % calculation
select chap4a.landing_page,
count(distinct chap4a.website_session_id) as website_session_count,
count(distinct chap4_bouncedsessions.website_session_id) as bounced_session_count,
count(distinct chap4_bouncedsessions.website_session_id)/ count(distinct chap4a.website_session_id) as bounce_rate
from chap4a left join chap4_bouncedsessions 
on chap4a.website_session_id = chap4_bouncedsessions.website_session_id
group by 1
order by 2; 

-- assignment, calculate session count, bounced session count and bounce rate for homepage
-- step 1 - find first pageview id for relevant sessions
-- step 2 - identiy the landing page
-- step 3 - counting pageview for each session to understand bounced sessions
-- step 4 - summarizing and calculating bounce rate

select
website_session_id,
min(website_pageview_id) as first_pageview_id
from website_pageviews
where created_at < '2012-06-14'
group by 1;


create temporary table himani1
select
website_session_id,
min(website_pageview_id) as first_pageview_id
from website_pageviews
where created_at < '2012-06-14'
group by 1;

select * from himani1;


select himani1.website_session_id,
website_pageviews.pageview_url
from himani1 
left join website_pageviews 
on himani1.website_session_id = website_pageviews.website_session_id
where pageview_url = '/home'
group by 1;


create temporary table himaniagg
select himani1.website_session_id,
website_pageviews.pageview_url
from himani1 
left join website_pageviews 
on himani1.website_session_id = website_pageviews.website_session_id
where pageview_url = '/home'
group by 1;

select * from himaniagg;

select
himaniagg.website_session_id,
himaniagg.pageview_url,
count(website_pageviews.website_pageview_id) as pageview_count
from himaniagg left join website_pageviews
on himaniagg.website_session_id = website_pageviews.website_session_id
group by 1, 2
having pageview_count = 1;

create temporary table bounced_session
select
himaniagg.website_session_id,
himaniagg.pageview_url,
count(website_pageviews.website_pageview_id) as pageview_count
from himaniagg left join website_pageviews
on himaniagg.website_session_id = website_pageviews.website_session_id
group by 1, 2
having pageview_count = 1;

select * from bounced_session;

select 
himaniagg.pageview_url as url,
count( distinct himaniagg.website_session_id) as website_sessions,
count(distinct bounced_session.website_session_id) as bounced_sessions,
count(distinct bounced_session.website_session_id)/count(distinct himaniagg.website_session_id) as bounce_rate
from himaniagg left join bounced_session 
on himaniagg.website_session_id = bounced_session.website_session_id
group by 1;

-- Assignment - A/B test between homepage and lander1 - session count, bounced sessions and bounce rate

-- Presteps - find the date period 
select
pageview_url,
min(website_pageview_id) as firstpageviewid,
min(created_at) as startdate
from website_pageviews
where pageview_url = '/lander-1'
group by 1;

-- Step 1 - create a table for website session id and first pageview id for landing page

select
website_pageviews.website_session_id,
min(website_pageviews.website_pageview_id) as landingpage
from website_pageviews
inner join website_sessions
on website_pageviews.website_session_id = website_sessions.website_session_id
where website_pageviews.created_at between '2012-06-19' and '2012-07-27'
and website_sessions.utm_source = 'gsearch'
and website_sessions.utm_campaign = 'nonbrand'
group by 1;




create temporary table Step1a
select
website_pageviews.website_session_id,
min(website_pageviews.website_pageview_id) as landingpage
from website_pageviews
inner join website_sessions
on website_pageviews.website_session_id = website_sessions.website_session_id
where website_pageviews.created_at between '2012-06-19' and '2012-07-27'
and website_sessions.utm_source = 'gsearch'
and website_sessions.utm_campaign = 'nonbrand'
group by 1;


select * from step1a;


-- step 2 combine pageview url to the session id and filter for home and lander-1

select
Step1a.website_session_id,
website_pageviews.pageview_url,
Step1a.landingpage
from Step1a left join website_pageviews
on Step1a.landingpage = website_pageviews.website_pageview_id
where website_pageviews.pageview_url IN ('/home','/lander-1')
group by 1;

create temporary table step2
select
Step1a.website_session_id,
website_pageviews.pageview_url,
Step1a.landingpage
from Step1a left join website_pageviews
on Step1a.landingpage = website_pageviews.website_pageview_id
where website_pageviews.pageview_url IN ('/home','/lander-1')
group by 1;

-- step 3 
select
step2.pageview_url,
step2.website_session_id,
count(distinct website_pageviews.website_pageview_id) as pageviewcount
from step2 left join website_pageviews
on step2.website_session_id = website_pageviews.website_session_id
group by 2
having pageviewcount = 1;

create temporary table step3
select
step2.pageview_url,
step2.website_session_id,
count(distinct website_pageviews.website_pageview_id) as pageviewcount
from step2 left join website_pageviews
on step2.website_session_id = website_pageviews.website_session_id
group by 2
having pageviewcount = 1;

--
create temporary table test1
select
step2.pageview_url,
step2.website_session_id as session_count,
step3.website_session_id as bounced_count
from step2 left join step3 on step2.website_session_id = step3.website_session_id
group by 2, 3;

select test1.pageview_url,
count(distinct test1.session_count) as sessions,
count(distinct test1.bounced_count) as bounced,
count(distinct test1.bounced_count)/count(distinct test1.session_count) as bounced_rate
from test1
group by 1;


-- step 4 bounced rates for home and lander1

select
step2.pageview_url,
count(distinct step2.website_session_id) as session_count,
count(distinct step3.website_session_id) as bounced_sessions,
count(distinct step3.website_session_id)/count(distinct step2.website_session_id) as bounce_rate
from step2 left join step3 on step2.website_session_id = step3.website_session_id
group by 1;

select * from website_sessions;

select utm_source,
utm_campaign,
utm_content,
device_type
from website_sessions
group by 1,2,3,4;

-- landing page trend analysis

create temporary table testa
select
website_pageviews.website_session_id,
min(website_pageviews.website_pageview_id) as firstview_page,
count(distinct website_pageviews.website_pageview_id) as pageview_count
from website_pageviews inner join website_sessions
on website_pageviews.website_session_id = website_sessions.website_session_id
where website_pageviews.created_at between '2012-06-01' and '2012-08-31'
and website_sessions.utm_source = 'gsearch'
and website_sessions.utm_campaign = 'nonbrand'
group by 1;

select * from testa;

create temporary table testb
select 
testa.website_session_id,
testa.firstview_page,
testa.pageview_count,
website_pageviews.pageview_url,
website_pageviews.created_at
from testa left join website_pageviews on testa.firstview_page = website_pageviews.website_pageview_id
where website_pageviews.pageview_url IN ('/home', '/lander-1')
group by 1;

select * from testb;

create temporary table summary
select
week(created_at) as week,
min(created_at) as week_start_date,
count(distinct website_session_id) as total_sessions,
count(distinct case when pageview_count = 1 then website_session_id else null end) as bounced_session_count,
count(distinct case when pageview_url = '/home' then website_session_id else null end) as home,
count(distinct case when pageview_url = '/lander-1' then website_session_id else null end) as lander1
from testb
group by 1;

select* from summary;

select
week,
bounced_session_count/total_sessions,
home,
lander1
from summary;

-- my long way

create temporary table home
select 
testa.website_session_id,
testa.firstview_page,
testa.pageview_count,
website_pageviews.pageview_url,
website_pageviews.created_at
from testa left join website_pageviews on testa.firstview_page = website_pageviews.website_pageview_id
where website_pageviews.pageview_url = '/home'
group by 1;

create temporary table lander1
select 
testa.website_session_id,
testa.firstview_page,
testa.pageview_count,
website_pageviews.pageview_url,
website_pageviews.created_at
from testa left join website_pageviews on testa.firstview_page = website_pageviews.website_pageview_id
where website_pageviews.pageview_url = '/lander-1'
group by 1;

create temporary table testbounce2
select 
testb.website_session_id,
testb.firstview_page,
testb.pageview_count,
website_pageviews.pageview_url,
website_pageviews.created_at
from testb left join website_pageviews on testb.firstview_page = website_pageviews.website_pageview_id
group by 1
having testb.pageview_count = 1;

-- summary

select
week(testb.created_at),
min(testb.created_at) as start_date,
max(testb.created_at) as end_date,
count(distinct testbounce2.website_session_id)/count(distinct testb.website_session_id) as bounce_rate,
count(distinct home.website_session_id) as home,
count(distinct lander1.website_session_id) as lander1
from testb 
left join testbounce2 
on testb.website_session_id = testbounce2.website_session_id
left join home 
on testb.website_session_id = home.website_session_id 
left join lander1
on testb.website_session_id = lander1.website_session_id
group by 1;

-- Building conversion funnels and conversion paths

/*. Business Context - DEMO
A - build conversion mini funnel from lander-2 to cart
B - how many people reach each step and how many drop off
C - looking at lander-2 traffic
D - customers who like Mr Fuzzy only

Step 1 - pageviews for relevant sessions
Step 2 - identify each pageview session as the specific funnel step
Step 3 - create session level conversion funnel view
Step 4 -  aggregate data to assess funnel performance
*/

select * from website_pageviews;

select 
website_sessions.website_session_id,
website_pageviews.pageview_url,
website_sessions.created_at,
case when website_pageviews.pageview_url = '/products' then 1 else 0 end as product,
case when website_pageviews.pageview_url = '/the-original-mr-fuzzy' then 1 else 0 end as mrfuzzy,
case when website_pageviews.pageview_url = '/cart' then 1 else 0 end as cart
from website_sessions
left join website_pageviews
on website_sessions.website_session_id = website_pageviews.website_session_id
where website_sessions.created_at between '2014-01-01' and '2014-02-01'
and website_pageviews.pageview_url in ('/lander-2','/products','/the-original-mr-fuzzy','/cart')
order by 1,3;

create temporary table session_demo1
select
website_session_id,
max(product) as product,
max(mrfuzzy) as mrfuzzy,
max(cart) as cart
from (
select 
website_sessions.website_session_id,
website_pageviews.pageview_url,
website_sessions.created_at,
case when website_pageviews.pageview_url = '/products' then 1 else 0 end as product,
case when website_pageviews.pageview_url = '/the-original-mr-fuzzy' then 1 else 0 end as mrfuzzy,
case when website_pageviews.pageview_url = '/cart' then 1 else 0 end as cart
from website_sessions
left join website_pageviews
on website_sessions.website_session_id = website_pageviews.website_session_id
where website_sessions.created_at between '2014-01-01' and '2014-02-01'
and website_pageviews.pageview_url in ('/lander-2','/products','/the-original-mr-fuzzy','/cart')
order by 1,3) as pageview_level
group by 1;

select * from session_demo1;

-- counting the pageview sessions
select
count(distinct website_session_id),
count(case when product = 1 then website_session_id else null end) as products,
count(case when mrfuzzy = 1 then website_session_id else null end) as mrfuzzy,
count(case when cart = 1 then website_session_id else null end) as cart
from session_demo1;


-- calculating click through rates
select
count(distinct website_session_id),
count(case when product = 1 then website_session_id else null end)/count(distinct website_session_id) as productsCTR,
count(case when mrfuzzy = 1 then website_session_id else null end)/count(case when product = 1 then website_session_id else null end) as mrfuzzyCTR,
count(case when cart = 1 then website_session_id else null end)/count(case when mrfuzzy = 1 then website_session_id else null end) as cartCTR
from session_demo1;


-- assignmnet

create temporary table assg2
select
website_session_id,
max(lander1) as lander1,
max(products) as products,
max(mrfuzzy) as mrfuzzy,
max(cart) as cart,
max(shipping) as shipping,
max(billing) as billing,
max(thankyou_order) as thankyou_order
from (
select
website_sessions.website_session_id,
website_pageviews.pageview_url,
case when website_pageviews.pageview_url = '/lander-1' then 1 else 0 end as lander1,
case when website_pageviews.pageview_url = '/products' then 1 else 0 end as products,
case when website_pageviews.pageview_url = '/the-original-mr-fuzzy' then 1 else 0 end as mrfuzzy,
case when website_pageviews.pageview_url = '/cart' then 1 else 0 end as cart,
case when website_pageviews.pageview_url = '/shipping' then 1 else 0 end as shipping,
case when website_pageviews.pageview_url = '/billing' then 1 else 0 end as billing,
case when website_pageviews.pageview_url = '/thank-you-for-your-order' then 1 else 0 end as thankyou_order
from website_sessions 
left join website_pageviews
on website_sessions.website_session_id = website_pageviews.website_session_id
where website_sessions.created_at > '2012-08-05' and website_sessions.created_at < '2012-09-05'
and website_pageviews.pageview_url in ('/lander-1', '/products',
'/the-original-mr-fuzzy','/cart','/shipping','/billing','/thank-you-for-your-order')
and website_sessions.utm_source = 'gsearch'
and website_sessions.utm_campaign = 'nonbrand'
order by 1) as pageview_level
group by 1;




select * from assg1;
select * from assg2;

select 
count(distinct website_session_id),
count(distinct case when lander1 = 1 then website_session_id else null end) as lander1count,
count(distinct case when products = 1 then website_session_id else null end) as productcount,
count(distinct case when mrfuzzy = 1 then website_session_id else null end) as mrfuzzycount,
count(distinct case when cart = 1 then website_session_id else null end) as cartcount,
count(distinct case when shipping = 1 then website_session_id else null end) as shippingcount,
count(distinct case when billing = 1 then website_session_id else null end) as billingcount,
count(distinct case when thankyou_order = 1 then website_session_id else null end) as thankyoucount
from assg2;

select 
count(distinct website_session_id),
count(distinct case when lander1 = 1 then website_session_id else null end)/count(distinct website_session_id)
 as lander1CTR,
count(distinct case when products = 1 then website_session_id else null end)/count(distinct case when lander1 = 1 then website_session_id else null end)
 as productCTR,
count(distinct case when mrfuzzy = 1 then website_session_id else null end)/count(distinct case when products = 1 then website_session_id else null end)
 as mrfuzzyCTR,
count(distinct case when cart = 1 then website_session_id else null end)/count(distinct case when mrfuzzy = 1 then website_session_id else null end)
 as cartCTR,
count(distinct case when shipping = 1 then website_session_id else null end)/count(distinct case when cart = 1 then website_session_id else null end)
 as shippingCTR,
count(distinct case when billing = 1 then website_session_id else null end)/count(distinct case when shipping = 1 then website_session_id else null end)
 as billingCTR,
count(distinct case when thankyou_order = 1 then website_session_id else null end)/count(distinct case when billing = 1 then website_session_id else null end)
 as thankyouCTR
from assg2;


-- new assignment - chap4 - A/B split test 

select
website_pageview_id,
min(Date(created_at)) as start_date
from website_pageviews
where pageview_url = '/billing-2';

create temporary table funnelb
select website_session_id,
max(billing) as billing,
max(billing_2) as billing_2,
max(ty_order) as ty_order
from (select
website_sessions.website_session_id,
website_pageviews.pageview_url,
case when pageview_url = '/billing' then 1 else 0 end as billing,
case when pageview_url = '/billing-2' then 1 else 0 end as billing_2,
case when pageview_url = '/thank-you-for-your-order' then 1 else 0 end as ty_order
from website_sessions
left join website_pageviews
on website_sessions.website_session_id = website_pageviews.website_session_id
where website_sessions.created_at between '2012-09-10' and '2012-11-10'
and pageview_url in ('/billing', '/billing-2', '/thank-you-for-your-order')
order by 1) as pageview_info1
group by 1;

select * from funnelb;


select
billing2 as funnel_stage,
count(distinct website_session_id) as sessions,
count(distinct case when billing_2 = 1 then website_session_id else null end) as billing2sessions,
count(distinct case when billing_2 = 1 and ty_order = 1 then website_session_id else null end) as order2_billing,
count(distinct case when billing_2 = 1 and ty_order = 1 then website_session_id else null end)/count(distinct case when billing = 1 then website_session_id else null end)
as CTR_billing2
from funnelb;

select
billing as funnel_stage,
count(distinct website_session_id) as sessions,
count(distinct case when billing = 1 then website_session_id else null end) as billingsessions,
count(distinct case when billing = 1 and ty_order = 1 then website_session_id else null end) as order_billing,
count(distinct case when billing = 1 and ty_order = 1 then website_session_id else null end)/count(distinct case when billing = 1 then website_session_id else null end)
as CTR_billing
from funnelb;


-- solution video 
create temporary table solution
select website_pageviews.website_session_id,
website_pageviews.pageview_url,
orders.order_id
from website_pageviews left join orders on orders.website_session_id = website_pageviews.website_session_id
where website_pageviews.created_at between '2012-09-10' and '2012-11-10'
and website_pageviews.pageview_url in ('/billing', '/billing-2');

select pageview_url,
count(distinct website_session_id),
count(distinct order_id),
count(distinct order_id)/count(distinct website_session_id) as CTR
from solution
group by 1;

select * from website_pageviews;



