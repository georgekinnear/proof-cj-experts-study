-- phpMyAdmin SQL Dump
-- version 4.9.5
-- https://www.phpmyadmin.net/
--
-- Host: localhost:3306
-- Generation Time: Jun 11, 2021 at 03:41 PM
-- Server version: 5.6.51
-- PHP Version: 7.3.28

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT = 0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `hsn_shiny`
--

-- --------------------------------------------------------

--
-- Table structure for table `judgements`
--

CREATE TABLE `judgements` (
  `judgement_id` int(11) NOT NULL,
  `study` varchar(255) NOT NULL,
  `judge_id` int(11) NOT NULL,
  `left` varchar(10) NOT NULL,
  `right` varchar(10) NOT NULL,
  `won` varchar(10) NOT NULL,
  `lost` varchar(10) NOT NULL,
  `time_taken` int(11) NOT NULL,
  `left_comment` text,
  `right_comment` text
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `judgements`
--
ALTER TABLE `judgements`
  ADD PRIMARY KEY (`judgement_id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `judgements`
--
ALTER TABLE `judgements`
  MODIFY `judgement_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=482;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
