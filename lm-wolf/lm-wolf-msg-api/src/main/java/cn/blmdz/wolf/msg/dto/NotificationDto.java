package cn.blmdz.wolf.msg.dto;

import java.io.Serializable;
import java.util.List;

public class NotificationDto implements Serializable {
   private List<Long> audienceIds;
   private String audienceGroup1;
   private String audienceGroup2;
   private String audienceGroup3;
   private String audienceGroup4;
   private String subject;
   private String content;

   public List<Long> getAudienceIds() {
      return this.audienceIds;
   }

   public String getAudienceGroup1() {
      return this.audienceGroup1;
   }

   public String getAudienceGroup2() {
      return this.audienceGroup2;
   }

   public String getAudienceGroup3() {
      return this.audienceGroup3;
   }

   public String getAudienceGroup4() {
      return this.audienceGroup4;
   }

   public String getSubject() {
      return this.subject;
   }

   public String getContent() {
      return this.content;
   }

   public void setAudienceIds(List audienceIds) {
      this.audienceIds = audienceIds;
   }

   public void setAudienceGroup1(String audienceGroup1) {
      this.audienceGroup1 = audienceGroup1;
   }

   public void setAudienceGroup2(String audienceGroup2) {
      this.audienceGroup2 = audienceGroup2;
   }

   public void setAudienceGroup3(String audienceGroup3) {
      this.audienceGroup3 = audienceGroup3;
   }

   public void setAudienceGroup4(String audienceGroup4) {
      this.audienceGroup4 = audienceGroup4;
   }

   public void setSubject(String subject) {
      this.subject = subject;
   }

   public void setContent(String content) {
      this.content = content;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof NotificationDto)) {
         return false;
      } else {
         NotificationDto other = (NotificationDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$audienceIds = this.getAudienceIds();
            Object other$audienceIds = other.getAudienceIds();
            if(this$audienceIds == null) {
               if(other$audienceIds != null) {
                  return false;
               }
            } else if(!this$audienceIds.equals(other$audienceIds)) {
               return false;
            }

            Object this$audienceGroup1 = this.getAudienceGroup1();
            Object other$audienceGroup1 = other.getAudienceGroup1();
            if(this$audienceGroup1 == null) {
               if(other$audienceGroup1 != null) {
                  return false;
               }
            } else if(!this$audienceGroup1.equals(other$audienceGroup1)) {
               return false;
            }

            Object this$audienceGroup2 = this.getAudienceGroup2();
            Object other$audienceGroup2 = other.getAudienceGroup2();
            if(this$audienceGroup2 == null) {
               if(other$audienceGroup2 != null) {
                  return false;
               }
            } else if(!this$audienceGroup2.equals(other$audienceGroup2)) {
               return false;
            }

            Object this$audienceGroup3 = this.getAudienceGroup3();
            Object other$audienceGroup3 = other.getAudienceGroup3();
            if(this$audienceGroup3 == null) {
               if(other$audienceGroup3 != null) {
                  return false;
               }
            } else if(!this$audienceGroup3.equals(other$audienceGroup3)) {
               return false;
            }

            Object this$audienceGroup4 = this.getAudienceGroup4();
            Object other$audienceGroup4 = other.getAudienceGroup4();
            if(this$audienceGroup4 == null) {
               if(other$audienceGroup4 != null) {
                  return false;
               }
            } else if(!this$audienceGroup4.equals(other$audienceGroup4)) {
               return false;
            }

            Object this$subject = this.getSubject();
            Object other$subject = other.getSubject();
            if(this$subject == null) {
               if(other$subject != null) {
                  return false;
               }
            } else if(!this$subject.equals(other$subject)) {
               return false;
            }

            Object this$content = this.getContent();
            Object other$content = other.getContent();
            if(this$content == null) {
               if(other$content != null) {
                  return false;
               }
            } else if(!this$content.equals(other$content)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof NotificationDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $audienceIds = this.getAudienceIds();
      result = result * 31 + ($audienceIds == null?0:$audienceIds.hashCode());
      Object $audienceGroup1 = this.getAudienceGroup1();
      result = result * 31 + ($audienceGroup1 == null?0:$audienceGroup1.hashCode());
      Object $audienceGroup2 = this.getAudienceGroup2();
      result = result * 31 + ($audienceGroup2 == null?0:$audienceGroup2.hashCode());
      Object $audienceGroup3 = this.getAudienceGroup3();
      result = result * 31 + ($audienceGroup3 == null?0:$audienceGroup3.hashCode());
      Object $audienceGroup4 = this.getAudienceGroup4();
      result = result * 31 + ($audienceGroup4 == null?0:$audienceGroup4.hashCode());
      Object $subject = this.getSubject();
      result = result * 31 + ($subject == null?0:$subject.hashCode());
      Object $content = this.getContent();
      result = result * 31 + ($content == null?0:$content.hashCode());
      return result;
   }

   public String toString() {
      return "NotificationDto(audienceIds=" + this.getAudienceIds() + ", audienceGroup1=" + this.getAudienceGroup1() + ", audienceGroup2=" + this.getAudienceGroup2() + ", audienceGroup3=" + this.getAudienceGroup3() + ", audienceGroup4=" + this.getAudienceGroup4() + ", subject=" + this.getSubject() + ", content=" + this.getContent() + ")";
   }
}
