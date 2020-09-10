USE GA
CREATE TABLE GoogleAnalyticsViewingID
([sn]int null,
[Viewingid][nvarchar](20) NULL,
[WebName][nvarchar](20) NULL,
[WebStartDate][date]NULL
)
insert into [GA].[dbo].[GoogleAnalyticsViewingID] values
(1,'184032350','FJ002','2018-11-15'),
(2,'184062247','EH001','2018-11-15'),
(3,'184055075','EH002','2018-11-15'),
(4,'184006186','EH003','2018-11-15'),
(5,'184041744','CJ004','2018-11-15'),
(6,'140281091','CJ001','2018-11-15'),
(7,'140290543','BA003','2018-11-15'),
(8,'140358844','BZ001','2018-11-15'),
(9,'140353456','BZ008','2018-11-15'),
(10,'184038927','BZ004','2018-11-15'),
(11,'140301601','BA002','2018-11-15'),
(12,'184032149','DQ002','2018-11-15'),
(13,'140275181','DQ001','2018-11-15'),
(14,'140953836','BM001','2018-11-15'),
(15,'140411602','BW003','2018-07-01'),
(16,'140413602','BW004','2018-07-01'),
(17,'140377426','FJ001','2018-07-01'),
(18,'140417405','BW007','2018-07-01'),
(19,'140355770','BZ003','2018-07-01'),
(20,'140253977','BX001','2018-07-01')

CREATE TABLE GoogleAnalyticsDailyRecord
([web][nvarchar](20) NULL,
[pagePath][nvarchar](2500)null,
[landingPagePath][nvarchar](2500)null,
[userType][nvarchar](20)null,
[date][date]NULL,
[deviceCategory][nvarchar](20)null,
[sessions] int,
[users] int,
[pageviews]int
)

CREATE TABLE GoogleAnalyticsLandingPathResult
([web][nvarchar](20) NULL,
[Date][date]NULL,
[LandingPagePath][nvarchar](2500)null,
[UserType][nvarchar](20)null,
[Sessions]int,
[Users]int
)

CREATE TABLE GoogleAnalyticsAllPageResult
([web][nvarchar](20) NULL,
[Date][date]NULL,
[PagePath][nvarchar](2500)null,
[UserType][nvarchar](20)null,
[Sessions]int,
[Users]int
)


