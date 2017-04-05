package io.terminus.parana.article.enums;

import com.google.common.base.Objects;

public enum ArticleStatus {
   PUBLISHED(1, "已发布"),
   UNPUBLISHED(0, "未发布"),
   UNUSED(-1, "已停用"),
   DELETED(-2, "删除");

   private final int value;
   private final String desc;

   private ArticleStatus(int number, String desc) {
      this.value = number;
      this.desc = desc;
   }

   public static ArticleStatus from(int value) {
      for(ArticleStatus status : values()) {
         if(Objects.equal(Integer.valueOf(status.value), Integer.valueOf(value))) {
            return status;
         }
      }

      return null;
   }

   public int value() {
      return this.value;
   }

   public String toString() {
      return this.desc;
   }
}
