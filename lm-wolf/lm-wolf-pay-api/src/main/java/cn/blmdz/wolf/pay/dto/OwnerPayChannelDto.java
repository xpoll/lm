package cn.blmdz.wolf.pay.dto;

import java.io.Serializable;
import java.util.List;

public class OwnerPayChannelDto implements Serializable {
   private static final long serialVersionUID = -2534481074020296290L;
   private Long id;
   private List payChannels;

   public Long getId() {
      return this.id;
   }

   public List getPayChannels() {
      return this.payChannels;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setPayChannels(List payChannels) {
      this.payChannels = payChannels;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof OwnerPayChannelDto)) {
         return false;
      } else {
         OwnerPayChannelDto other = (OwnerPayChannelDto)o;
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

            Object this$payChannels = this.getPayChannels();
            Object other$payChannels = other.getPayChannels();
            if(this$payChannels == null) {
               if(other$payChannels != null) {
                  return false;
               }
            } else if(!this$payChannels.equals(other$payChannels)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof OwnerPayChannelDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $payChannels = this.getPayChannels();
      result = result * 31 + ($payChannels == null?0:$payChannels.hashCode());
      return result;
   }

   public String toString() {
      return "OwnerPayChannelDto(id=" + this.getId() + ", payChannels=" + this.getPayChannels() + ")";
   }
}
