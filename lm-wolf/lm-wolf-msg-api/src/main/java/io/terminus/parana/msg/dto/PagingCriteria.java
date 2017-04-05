package io.terminus.parana.msg.dto;

import io.terminus.common.model.PageInfo;
import java.io.Serializable;

public class PagingCriteria implements Serializable {
   private static final long serialVersionUID = 2598875146576926658L;
   private Long id;
   private Integer pageNo;
   private Integer pageSize;

   public Integer getLimit() {
      PageInfo pageInfo = new PageInfo(this.pageNo, this.pageSize);
      return pageInfo.getLimit();
   }

   public Integer getOffset() {
      PageInfo pageInfo = new PageInfo(this.pageNo, this.pageSize);
      return pageInfo.getOffset();
   }

   public Long getId() {
      return this.id;
   }

   public Integer getPageNo() {
      return this.pageNo;
   }

   public Integer getPageSize() {
      return this.pageSize;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setPageNo(Integer pageNo) {
      this.pageNo = pageNo;
   }

   public void setPageSize(Integer pageSize) {
      this.pageSize = pageSize;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof PagingCriteria)) {
         return false;
      } else {
         PagingCriteria other = (PagingCriteria)o;
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

            Object this$pageNo = this.getPageNo();
            Object other$pageNo = other.getPageNo();
            if(this$pageNo == null) {
               if(other$pageNo != null) {
                  return false;
               }
            } else if(!this$pageNo.equals(other$pageNo)) {
               return false;
            }

            Object this$pageSize = this.getPageSize();
            Object other$pageSize = other.getPageSize();
            if(this$pageSize == null) {
               if(other$pageSize != null) {
                  return false;
               }
            } else if(!this$pageSize.equals(other$pageSize)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof PagingCriteria;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $pageNo = this.getPageNo();
      result = result * 31 + ($pageNo == null?0:$pageNo.hashCode());
      Object $pageSize = this.getPageSize();
      result = result * 31 + ($pageSize == null?0:$pageSize.hashCode());
      return result;
   }

   public String toString() {
      return "PagingCriteria(id=" + this.getId() + ", pageNo=" + this.getPageNo() + ", pageSize=" + this.getPageSize() + ")";
   }
}
