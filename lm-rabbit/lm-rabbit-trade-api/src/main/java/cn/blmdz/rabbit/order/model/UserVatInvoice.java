package cn.blmdz.rabbit.order.model;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * Desc: 增值税开票信息
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
@Data
public class UserVatInvoice implements Serializable {

    private static final long serialVersionUID = -1840014075570454146L;

    private Long id;                        // id 主键

    private Long userId;                    // 用户id

    private String companyName;             // 公司名称

    private String taxRegisterNo;           // 税务登记号

    private String registerAddress;         // 注册地址

    private String registerPhone;           // 注册电话

    private String registerBank;            // 注册银行

    private String bankAccount;             // 银行帐号

    private String taxCertificate;          // 税务登记证

    private String taxpayerCertificate;     // 一般纳税人证书

    private Date createdAt;                 // 创建时间

    private Date updatedAt;                 // 更新时间
}
