package io.terminus.lib.pay.channel.unionpay.request;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.JsonMapper;
import io.terminus.lib.pay.channel.unionpay.request.Request;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.channel.unionpay.sdk.SDKConfig;
import io.terminus.lib.pay.utils.ZipUtils;
import java.io.File;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UnionQueryTradeRequest extends Request {
   private static final Logger log = LoggerFactory.getLogger(UnionQueryTradeRequest.class);
   public static final Splitter SQUARE = Splitter.on("^").omitEmptyStrings().trimResults();

   private UnionQueryTradeRequest(UnionToken token) {
      super(token);
      this.params.put("txnType", "76");
      this.params.put("txnSubType", "01");
   }

   public static UnionQueryTradeRequest build(UnionToken token) {
      return new UnionQueryTradeRequest(token);
   }

   public UnionQueryTradeRequest bizType(String bizType) {
      if(Arguments.notNull(bizType)) {
         this.params.put("bizType", bizType);
      }

      return this;
   }

   public UnionQueryTradeRequest settleDate(String settleDate) {
      if(Arguments.notEmpty(settleDate)) {
         this.params.put("settleDate", settleDate);
      }

      return this;
   }

   public UnionQueryTradeRequest fileType(String fileType) {
      this.params.put("fileType", fileType);
      return this;
   }

   public Response query() {
      Response<List<Map<Integer, String>>> response = new Response();
      Map<String, String> submitFromData = signData(this.param());
      String url = this.unionToken.getFileTransUrl();
      Map<String, String> resmap = submitUrl(submitFromData, url);
      if(((String)resmap.get("respCode")).equals("00")) {
         deCodeFileContent(resmap, SDKConfig.getConfig().getTransFilePatch());
         response.setResult(this.getDate(resmap, SDKConfig.getConfig().getTransFilePatch()));
      } else if(((String)resmap.get("respCode")).equals("98")) {
         List<Map<Integer, String>> maps = Lists.newArrayList();
         response.setResult(maps);
      } else {
         response.setError("query.fail");
      }

      return response;
   }

   public String queryParam() {
      Map<String, String> submitFromData = signData(this.param());
      return JsonMapper.nonEmptyMapper().toJson(submitFromData);
   }

   public List getDate(Map resmap, String patch) {
      String fileName = (String)resmap.get("fileName");
      String folderName = fileName.substring(0, fileName.indexOf("."));

      try {
         ZipUtils.decompress(patch + fileName, patch + folderName);
      } catch (Exception var9) {
         var9.printStackTrace();
      }

      File file = new File(patch + folderName);
      File[] tempList = file.listFiles();
      String filePatch = "";

      for(int i = 0; i < tempList.length; ++i) {
         if(tempList[i].isFile() && tempList[i].getName().contains("INN")) {
            filePatch = tempList[i].toString();
            break;
         }
      }

      return parseZMFile(filePatch);
   }
}
