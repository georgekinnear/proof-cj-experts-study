-- phpMyAdmin SQL Dump
-- version 4.9.5
-- https://www.phpmyadmin.net/
--
-- Host: localhost:3306
-- Generation Time: Jun 11, 2021 at 03:40 PM
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
-- Table structure for table `studies`
--

CREATE TABLE `studies` (
  `study` varchar(21) DEFAULT NULL,
  `definition_prompt` varchar(113) DEFAULT NULL,
  `judging_prompt` varchar(62) DEFAULT NULL,
  `target_judges` smallint(6) NOT NULL DEFAULT '25',
  `min_per_judge` smallint(6) NOT NULL DEFAULT '10'
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Dumping data for table `studies`
--

INSERT INTO `studies` (`study`, `definition_prompt`, `judging_prompt`, `target_judges`, `min_per_judge`) VALUES
('experts_rigour', 'Please explain briefly in a few sentences what it means for a proof to be rigorous.', 'Which proof is more rigorous?', 30, 10),
('experts_insight', 'Please explain briefly in a few sentences what it means for a proof to give insight into why the theorem is true.', 'Which proof gives more insight into why the theorem is true?', 30, 10),
('experts_simple', 'Please explain briefly in a few sentences what it means for a proof to be simple.', 'Which proof is the simplest?', 30, 10),
('experts_understanding', 'Please explain briefly in a few sentences what it means for a proof to help you understand why a theorem is true.', 'Which proof best helps you understand why the theorem is true?', 30, 10),
('experts_marks', 'Please explain briefly in a few sentences what it means for a proof to be worth high marks in an assessment.', 'Which proof would get the most marks in an assessment?', 30, 10),
('phd_rigour', 'Please explain briefly in a few sentences what it means for a proof to be rigorous.', 'Which proof is more rigorous?', 30, 10),
('phd_insight', 'Please explain briefly in a few sentences what it means for a proof to give insight into why the theorem is true.', 'Which proof gives more insight into why the theorem is true?', 30, 10),
('phd_simple', 'Please explain briefly in a few sentences what it means for a proof to be simple.', 'Which proof is the simplest?', 30, 10),
('phd_understanding', 'Please explain briefly in a few sentences what it means for a proof to help you understand why a theorem is true.', 'Which proof best helps you understand why the theorem is true?', 30, 10),
('phd_marks', 'Please explain briefly in a few sentences what it means for a proof to be worth high marks in an assessment.', 'Which proof would get the most marks in an assessment?', 30, 10),
('other_rigour', 'Please explain briefly in a few sentences what it means for a proof to be rigorous.', 'Which proof is more rigorous?', 25, 10),
('other_insight', 'Please explain briefly in a few sentences what it means for a proof to give insight into why the theorem is true.', 'Which proof gives more insight into why the theorem is true?', 25, 10),
('other_simple', 'Please explain briefly in a few sentences what it means for a proof to be simple.', 'Which proof is the simplest?', 25, 10),
('other_understanding', 'Please explain briefly in a few sentences what it means for a proof to help you understand why a theorem is true.', 'Which proof best helps you understand why the theorem is true?', 25, 10),
('other_marks', 'Please explain briefly in a few sentences what it means for a proof to be worth high marks in an assessment.', 'Which proof would get the most marks in an assessment?', 25, 10);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
