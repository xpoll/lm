package cn.blmdz.aide.pay.channel.kjtpay.dto;

import java.io.Serializable;

import lombok.Data;

@Data
public class TransferToBankInfo implements Serializable {
	private static final long serialVersionUID = -8842382735766960880L;
	private String outTradeNum;
	private String accountName;
	private String accountNo;
	private String bankNo;
	private String bankName;
	private String bankBranchName;
	private String bankBranchNo = "";
	private String bankProvice;
	private String bankCity;
	private String accountType;
	private String amount;
	private String arriveAccontLevel = "";
	private String memo = "";
	private String description;
}
