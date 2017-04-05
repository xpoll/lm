package io.terminus.parana.user.address.model;

import java.io.Serializable;

public class Address implements Serializable {
   private static final long serialVersionUID = -7496545307904479697L;
   private Integer id;
   private Integer pid;
   private String name;
   private Integer level;
   private String pinyin;
   private String englishName;
   private String unicodeCode;
   private String orderNo;

   public String toString() {
      return "Address(id=" + this.getId() + ", pid=" + this.getPid() + ", name=" + this.getName() + ", level=" + this.getLevel() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Address)) {
         return false;
      } else {
         Address other = (Address)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$pid = this.getPid();
            Object other$pid = other.getPid();
            if(this$pid == null) {
               if(other$pid != null) {
                  return false;
               }
            } else if(!this$pid.equals(other$pid)) {
               return false;
            }

            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$level = this.getLevel();
            Object other$level = other.getLevel();
            if(this$level == null) {
               if(other$level != null) {
                  return false;
               }
            } else if(!this$level.equals(other$level)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Address;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $pid = this.getPid();
      result = result * 59 + ($pid == null?0:$pid.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $level = this.getLevel();
      result = result * 59 + ($level == null?0:$level.hashCode());
      return result;
   }

   public Integer getId() {
      return this.id;
   }

   public void setId(Integer id) {
      this.id = id;
   }

   public Integer getPid() {
      return this.pid;
   }

   public void setPid(Integer pid) {
      this.pid = pid;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public Integer getLevel() {
      return this.level;
   }

   public void setLevel(Integer level) {
      this.level = level;
   }

   public String getPinyin() {
      return this.pinyin;
   }

   public void setPinyin(String pinyin) {
      this.pinyin = pinyin;
   }

   public String getEnglishName() {
      return this.englishName;
   }

   public void setEnglishName(String englishName) {
      this.englishName = englishName;
   }

   public String getUnicodeCode() {
      return this.unicodeCode;
   }

   public void setUnicodeCode(String unicodeCode) {
      this.unicodeCode = unicodeCode;
   }

   public String getOrderNo() {
      return this.orderNo;
   }

   public void setOrderNo(String orderNo) {
      this.orderNo = orderNo;
   }
}
