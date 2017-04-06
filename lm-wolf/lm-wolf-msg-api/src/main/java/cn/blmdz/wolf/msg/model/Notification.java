package cn.blmdz.wolf.msg.model;

import java.io.Serializable;
import java.util.Date;

public class Notification implements Serializable {
   private static final long serialVersionUID = -6335190925186199777L;
   private Long id;
   private Long audienceId;
   private String audienceGroup1;
   private String audienceGroup2;
   private String audienceGroup3;
   private String audienceGroup4;
   private String subject;
   private String content;
   private Boolean checked;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getAudienceId() {
      return this.audienceId;
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

   public Boolean getChecked() {
      return this.checked;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setAudienceId(Long audienceId) {
      this.audienceId = audienceId;
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

   public void setChecked(Boolean checked) {
      this.checked = checked;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Notification)) {
         return false;
      } else {
         Notification other = (Notification)o;
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

            Object this$audienceId = this.getAudienceId();
            Object other$audienceId = other.getAudienceId();
            if(this$audienceId == null) {
               if(other$audienceId != null) {
                  return false;
               }
            } else if(!this$audienceId.equals(other$audienceId)) {
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

            Object this$checked = this.getChecked();
            Object other$checked = other.getChecked();
            if(this$checked == null) {
               if(other$checked != null) {
                  return false;
               }
            } else if(!this$checked.equals(other$checked)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Notification;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $audienceId = this.getAudienceId();
      result = result * 31 + ($audienceId == null?0:$audienceId.hashCode());
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
      Object $checked = this.getChecked();
      result = result * 31 + ($checked == null?0:$checked.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "Notification(id=" + this.getId() + ", audienceId=" + this.getAudienceId() + ", audienceGroup1=" + this.getAudienceGroup1() + ", audienceGroup2=" + this.getAudienceGroup2() + ", audienceGroup3=" + this.getAudienceGroup3() + ", audienceGroup4=" + this.getAudienceGroup4() + ", subject=" + this.getSubject() + ", content=" + this.getContent() + ", checked=" + this.getChecked() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
