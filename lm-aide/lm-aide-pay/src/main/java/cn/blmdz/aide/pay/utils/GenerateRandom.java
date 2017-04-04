package cn.blmdz.aide.pay.utils;

import java.util.Random;

public class GenerateRandom {
	public static String rand(int n) {
		String random = "";

		for (int i = 0; i < n; ++i) {
			Integer rd = Integer.valueOf((new Random()).nextInt(10));
			random = random + rd;
		}

		return random;
	}
}
