package cn.blmdz.wolf.web.core.component;

import com.google.common.base.Joiner;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.Arguments;

public class AliyunImageProperties {
   private static final Joiner UNDER_LINE = Joiner.on('_').skipNulls();
   private static final Object[] EMPTY_PARAM = new Object[0];
   private boolean normal = true;
   private Object[] params;
   private String high;
   private String width;
   private String percent;
   private String edge;
   private String largeThan;
   private String quality;
   private String rotate;
   private String opt;

   public AliyunImageProperties(Object[] params) {
      this.setParams(params);
   }

   public String getQueryString() {
      return "@" + UNDER_LINE.join(this.getParameters());
   }

   public String[] getParameters() {
      return new String[]{this.high, this.width, this.percent, this.edge, this.largeThan, this.quality, this.rotate, this.opt};
   }

   public void setHigh(String high) {
      if(Arguments.isDecimal(high)) {
         this.high = high + "h";
      } else {
         this.high = this.checkFormat(high, "h");
      }
   }

   public void setWidth(String width) {
      if(Arguments.isDecimal(width)) {
         this.width = width + "w";
      } else {
         this.width = this.checkFormat(width, "w");
      }
   }

   public void setPercent(String percent) {
      this.percent = this.checkFormat(percent, "p");
   }

   public void setEdge(String edge) {
      this.edge = this.checkFormat(edge, "e");
   }

   public void setLargeThan(String large) {
      this.largeThan = this.checkBitFormat(large, "l");
   }

   public void setQuality(String quality) {
      if(quality.endsWith("q")) {
         this.quality = this.checkFormat(quality, "q");
      } else {
         this.checkFormat(quality, "Q");
      }

   }

   public void setRotate(String rotate) {
      this.rotate = this.checkFormat(rotate, "r");
   }

   public void setOpt(String opt) {
      this.opt = this.checkFormat(opt, "o");
   }

   public void setParams(Object[] params) {
      if(params != null && params.length != 0) {
         this.params = params;
      } else {
         this.normal = false;
         this.params = EMPTY_PARAM;
      }

      this.procParams();
   }

   private void procParams() {
      for(int i = 0; i < this.params.length; ++i) {
         String param = this.params[i].toString();
         boolean bare = Arguments.isDecimal(param);
         if(bare) {
            if(i == 0) {
               this.setWidth(param);
            } else {
               if(i != 1) {
                  continue;
               }

               this.setHigh(param);
            }
         }

         switch(param.charAt(param.length() - 1)) {
         case 'Q':
         case 'q':
            this.setQuality(param);
            break;
         case 'e':
            this.setEdge(param);
            break;
         case 'h':
            this.setHigh(param);
            break;
         case 'l':
            this.setLargeThan(param);
            break;
         case 'o':
            this.setOpt(param);
            break;
         case 'p':
            this.setPercent(param);
            break;
         case 'r':
            this.setRotate(param);
            break;
         case 'w':
            this.setWidth(param);
         }
      }

   }

   private String checkBitFormat(String param, String suffix) {
      return !this.normal || Strings.isNullOrEmpty(param) || !param.endsWith(suffix) || param.charAt(0) != 48 && param.charAt(0) != 49?null:param;
   }

   private String checkFormat(String param, String suffix) {
      return this.normal && !Strings.isNullOrEmpty(param) && param.endsWith(suffix) && Arguments.isDecimal(param.substring(0, param.length() - 1))?param:null;
   }

   public static AliyunImageProperties from(Object... params) {
      return new AliyunImageProperties(params);
   }
}
