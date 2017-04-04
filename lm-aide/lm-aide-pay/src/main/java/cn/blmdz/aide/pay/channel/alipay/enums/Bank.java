package cn.blmdz.aide.pay.channel.alipay.enums;

import java.util.Collections;
import java.util.List;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.exception.BankNotFoundException;

public enum Bank {
   SHBANK("SHBANK", "上海银行", ""),
   BOC("BOCB2C", "中国银行", ""),
   ICBC("ICBCB2C", "中国工商银行", ""),
   ABC("ABC", "中国农业银行", ""),
   CMB("CMB", "招商银行", ""),
   CIB("CIB", "兴业银行", ""),
   CITIC("CITIC", "中信银行", ""),
   CEB("CEB-DEBIT", "中国光大银行", ""),
   PSBC("POSTGC", "中国邮政储蓄银行", ""),
   CCB("CCB", "中国建设银行", ""),
   SPABANK("SPABANK", "平安银行", ""),
   HZCB("HZCBB2C", "杭州银行", ""),
   FDB("FDB", "富滇银行", ""),
   BJRCB("BJRCB", "北京农商银行", ""),
   SPDB("SPDB", "上海浦发银行", ""),
   NBBANK("NBBANK", "宁波银行", ""),
   CMBC("CMBC", "中国民生银行", ""),
   GDB("GDB", "广发银行", "");

   private final String value;
   private final String display;
   private final String icon;

   private Bank(String value, String display, String icon) {
      this.value = value;
      this.display = display;
      this.icon = icon;
   }

   public static Bank from(String value) {
      for(Bank bank : values()) {
         if(Objects.equal(bank.value(), value)) {
            return bank;
         }
      }

      throw new BankNotFoundException("bank not found by value: " + value);
   }

   public static List<Bank> getBanks() {
      List<Bank> knownList = Lists.newArrayList();
      Collections.addAll(knownList, values());
      return knownList;
   }

   public String value() {
      return this.value;
   }

   public String icon() {
      return this.icon;
   }

   public String toString() {
      return this.display;
   }
}
