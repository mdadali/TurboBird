{$A+}

unit Speech;

interface

uses Windows, ActiveX;

{$HPPEMIT '#include <speech.h>'}

//UNICODE
{$HPPEMIT 'typedef System::DelphiInterface<IAttributesW> _di_IAttributesW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ILexPronounceW> _di_ILexPronounceW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRAttributesW> _di_ISRAttributesW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRCentralW> _di_ISRCentralW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRDialogsW> _di_ISRDialogsW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISREnumW> _di_ISREnumW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRFindW> _di_ISRFindW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramCommonW> _di_ISRGramCommonW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramCFGW> _di_ISRGramCFGW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramDictationW> _di_ISRGramDictationW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramNotifySinkW> _di_ISRGramNotifySinkW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResBasicW> _di_ISRResBasicW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResCorrectionW> _di_ISRResCorrectionW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResGraphW> _di_ISRResGraphW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResSpeakerW> _di_ISRResSpeakerW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRSpeakerW> _di_ISRSpeakerW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSAttributesW> _di_ITTSAttributesW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSCentralW> _di_ITTSCentralW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSDialogsW> _di_ITTSDialogsW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSEnumW> _di_ITTSEnumW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSFindW> _di_ITTSFindW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSNotifySinkW> _di_ITTSNotifySinkW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdNotifySinkW> _di_IVCmdNotifySinkW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdEnumW> _di_IVCmdEnumW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEnumSRShareW> _di_IEnumSRShareW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdMenuW> _di_IVCmdMenuW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVoiceCmdW> _di_IVoiceCmdW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdAttributesW> _di_IVCmdAttributesW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdDialogsW> _di_IVCmdDialogsW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVTxtNotifySinkW> _di_IVTxtNotifySinkW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVoiceTextW> _di_IVoiceTextW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVTxtAttributesW> _di_IVTxtAttributesW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVTxtDialogsW> _di_IVTxtDialogsW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctNotifySinkW> _di_IVDctNotifySinkW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVoiceDictationW> _di_IVoiceDictationW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctTextW> _di_IVDctTextW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctInvTextNormW> _di_IVDctInvTextNormW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctAttributesW> _di_IVDctAttributesW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctCommandsW> _di_IVDctCommandsW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctGlossaryW> _di_IVDctGlossaryW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctDialogsW> _di_IVDctDialogsW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctCommandsBuiltInW> _di_IVDctCommandsBuiltInW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctCommandsAppW> _di_IVDctCommandsAppW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISpchErrorW> _di_ISpchErrorW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ILexPronounce2W> _di_ILexPronounce2W;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRDialogs2W> _di_ISRDialogs2W;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramLexPronW> _di_ISRGramLexPronW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRSpeaker2W> _di_ISRSpeaker2W;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSNotifySink2W> _di_ITTSNotifySink2W;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctNotifySink2W> _di_IVDctNotifySink2W;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctText2W> _di_IVDctText2W;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSExternalSynthesizerW> _di_ITTSExternalSynthesizerW;'}
//ANSI
{$HPPEMIT 'typedef System::DelphiInterface<IAttributesA> _di_IAttributesA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ILexPronounceA> _di_ILexPronounceA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRAttributesA> _di_ISRAttributesA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRCentralA> _di_ISRCentralA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRDialogsA> _di_ISRDialogsA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISREnumA> _di_ISREnumA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRFindA> _di_ISRFindA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramCommonA> _di_ISRGramCommonA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramCFGA> _di_ISRGramCFGA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramDictationA> _di_ISRGramDictationA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramNotifySinkA> _di_ISRGramNotifySinkA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResBasicA> _di_ISRResBasicA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResCorrectionA> _di_ISRResCorrectionA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResGraphA> _di_ISRResGraphA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResSpeakerA> _di_ISRResSpeakerA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRSpeakerA> _di_ISRSpeakerA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSAttributesA> _di_ITTSAttributesA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSCentralA> _di_ITTSCentralA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSDialogsA> _di_ITTSDialogsA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSEnumA> _di_ITTSEnumA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSFindA> _di_ITTSFindA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSNotifySinkA> _di_ITTSNotifySinkA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdNotifySinkA> _di_IVCmdNotifySinkA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdEnumA> _di_IVCmdEnumA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEnumSRShareA> _di_IEnumSRShareA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdMenuA> _di_IVCmdMenuA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVoiceCmdA> _di_IVoiceCmdA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdAttributesA> _di_IVCmdAttributesA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVCmdDialogsA> _di_IVCmdDialogsA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVTxtNotifySinkA> _di_IVTxtNotifySinkA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVoiceTextA> _di_IVoiceTextA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVTxtAttributesA> _di_IVTxtAttributesA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVTxtDialogsA> _di_IVTxtDialogsA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctNotifySinkA> _di_IVDctNotifySinkA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVoiceDictationA> _di_IVoiceDictationA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctTextA> _di_IVDctTextA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctInvTextNormA> _di_IVDctInvTextNormA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctAttributesA> _di_IVDctAttributesA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctCommandsA> _di_IVDctCommandsA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctGlossaryA> _di_IVDctGlossaryA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctDialogsA> _di_IVDctDialogsA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctCommandsBuiltInA> _di_IVDctCommandsBuiltInA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctCommandsAppA> _di_IVDctCommandsAppA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRDialogs2A> _di_ISRDialogs2A;'}
{$HPPEMIT 'typedef System::DelphiInterface<ILexPronounce2A> _di_ILexPronounce2A;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISpchErrorA> _di_ISpchErrorA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramLexPronA> _di_ISRGramLexPronA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRSpeaker2A> _di_ISRSpeaker2A;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSNotifySink2A> _di_ITTSNotifySink2A;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctNotifySink2A> _di_IVDctNotifySink2A;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctText2A> _di_IVDctText2A;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSExternalSynthesizerA> _di_ITTSExternalSynthesizerA;'}
// NOT Specific to UNICODE or ANSI
{$HPPEMIT 'typedef System::DelphiInterface<IAudio> _di_IAudio;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioDest> _di_IAudioDest;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioDestNotifySink> _di_IAudioDestNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioMultiMediaDevice> _di_IAudioMultiMediaDevice;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioSource> _di_IAudioSource;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioSourceInstrumented> _di_IAudioSourceInstrumented;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioSourceSusp> _di_IAudioSourceSusp;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioSourceNotifySink> _di_IAudioSourceNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioFileNotifySink> _di_IAudioFileNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioFile> _di_IAudioFile;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioDirect> _di_IAudioDirect;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRGramInsertionGUI> _di_ISRGramInsertionGUI;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRNotifySink> _di_ISRNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResScores> _di_ISRResScores;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResMerge> _di_ISRResMerge;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResAudio> _di_ISRResAudio;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResEval> _di_ISRResEval;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResMemory> _di_ISRResMemory;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResModifyGUI> _di_ISRResModifyGUI;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITTSBufNotifySink> _di_ITTSBufNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctGUI> _di_IVDctGUI;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTMicWizard> _di_ISTMicWizard;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTPhoneConv> _di_ISTPhoneConv;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTGramComp> _di_ISTGramComp;'}
{$HPPEMIT 'typedef System::DelphiInterface<IVDctTextCache> _di_IVDctTextCache;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTLexDlg> _di_ISTLexDlg;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRNotifySink2> _di_ISRNotifySink2;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResAudioEx> _di_ISRResAudioEx;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISRResGraphEx> _di_ISRResGraphEx;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTRecord> _di_ISTRecord;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTRecordNotifySink> _di_ISTRecordNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTTTSQueue> _di_ISTTTSQueue;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTTTSQueueNotifySink> _di_ISTTTSQueueNotifySink;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISTLog> _di_ISTLog;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioSourceLog> _di_IAudioSourceLog;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioTel> _di_IAudioTel;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAudioExternalSynthesizer> _di_IAudioExternalSynthesizer;'}
// Now make one without the W or A to reflect whats being used.
{$HPPEMIT '#ifdef UNICODE'}
{$HPPEMIT 'typedef _di_IAttributesW _di_IAttributes;'}
{$HPPEMIT 'typedef _di_ILexPronounceW _di_ILexPronounce;'}
{$HPPEMIT 'typedef _di_ISRAttributesW _di_ISRAttributes;'}
{$HPPEMIT 'typedef _di_ISRCentralW _di_ISRCentral;'}
{$HPPEMIT 'typedef _di_ISRDialogsW _di_ISRDialogs;'}
{$HPPEMIT 'typedef _di_ISREnumW _di_ISREnum;'}
{$HPPEMIT 'typedef _di_ISRFindW _di_ISRFind;'}
{$HPPEMIT 'typedef _di_ISRGramCommonW _di_ISRGramCommon;'}
{$HPPEMIT 'typedef _di_ISRGramCFGW _di_ISRGramCFG;'}
{$HPPEMIT 'typedef _di_ISRGramDictationW _di_ISRGramDictation;'}
{$HPPEMIT 'typedef _di_ISRGramNotifySinkW _di_ISRGramNotifySink;'}
{$HPPEMIT 'typedef _di_ISRResBasicW _di_ISRResBasic;'}
{$HPPEMIT 'typedef _di_ISRResCorrectionW _di_ISRResCorrection;'}
{$HPPEMIT 'typedef _di_ISRResGraphW _di_ISRResGraph;'}
{$HPPEMIT 'typedef _di_ISRResSpeakerW _di_ISRResSpeaker;'}
{$HPPEMIT 'typedef _di_ISRSpeakerW _di_ISRSpeaker;'}
{$HPPEMIT 'typedef _di_ITTSAttributesW _di_ITTSAttributes;'}
{$HPPEMIT 'typedef _di_ITTSCentralW _di_ITTSCentral;'}
{$HPPEMIT 'typedef _di_ITTSDialogsW _di_ITTSDialogs;'}
{$HPPEMIT 'typedef _di_ITTSEnumW _di_ITTSEnum;'}
{$HPPEMIT 'typedef _di_ITTSFindW _di_ITTSFind;'}
{$HPPEMIT 'typedef _di_ITTSNotifySinkW _di_ITTSNotifySink;'}
{$HPPEMIT 'typedef _di_IVCmdNotifySinkW _di_IVCmdNotifySink;'}
{$HPPEMIT 'typedef _di_IVCmdEnumW _di_IVCmdEnum;'}
{$HPPEMIT 'typedef _di_IEnumSRShareW _di_IEnumSRShare;'}
{$HPPEMIT 'typedef _di_IVCmdMenuW _di_IVCmdMenu;'}
{$HPPEMIT 'typedef _di_IVoiceCmdW _di_IVoiceCmd;'}
{$HPPEMIT 'typedef _di_IVCmdAttributesW _di_IVCmdAttributes;'}
{$HPPEMIT 'typedef _di_IVCmdDialogsW _di_IVCmdDialogs;'}
{$HPPEMIT 'typedef _di_IVTxtNotifySinkW _di_IVTxtNotifySink;'}
{$HPPEMIT 'typedef _di_IVoiceTextW _di_IVoiceText;'}
{$HPPEMIT 'typedef _di_IVTxtAttributesW _di_IVTxtAttributes;'}
{$HPPEMIT 'typedef _di_IVTxtDialogsW _di_IVTxtDialogs;'}
{$HPPEMIT 'typedef _di_IVDctNotifySinkW _di_IVDctNotifySink;'}
{$HPPEMIT 'typedef _di_IVoiceDictationW _di_IVoiceDictation;'}
{$HPPEMIT 'typedef _di_IVDctTextW _di_IVDctText;'}
{$HPPEMIT 'typedef _di_IVDctInvTextNormW _di_IVDctInvTextNorm;'}
{$HPPEMIT 'typedef _di_IVDctAttributesW _di_IVDctAttributes;'}
{$HPPEMIT 'typedef _di_IVDctCommandsW _di_IVDctCommands;'}
{$HPPEMIT 'typedef _di_IVDctGlossaryW _di_IVDctGlossary;'}
{$HPPEMIT 'typedef _di_IVDctDialogsW _di_IVDctDialogs;'}
{$HPPEMIT 'typedef _di_IVDctCommandsBuiltInW _di_IVDctCommandsBuiltIn;'}
{$HPPEMIT 'typedef _di_IVDctCommandsAppW _di_IVDctCommandsApp;'}
{$HPPEMIT 'typedef _di_ISpchErrorW _di_ISpchError;'}
{$HPPEMIT 'typedef _di_ILexPronounce2W _di_ILexPronounce2;'}
{$HPPEMIT 'typedef _di_ISRDialogs2W _di_ISRDialogs2;'}
{$HPPEMIT 'typedef _di_ISRGramLexPronW _di_ISRGramLexPron;'}
{$HPPEMIT 'typedef _di_ISRSpeaker2W _di_ISRSpeaker2;'}
{$HPPEMIT 'typedef _di_ITTSNotifySink2W _di_ITTSNotifySink2;'}
{$HPPEMIT 'typedef _di_IVDctNotifySink2W _di_IVDctNotifySink2;'}
{$HPPEMIT 'typedef _di_IVDctText2W _di_IVDctText2;'}
{$HPPEMIT 'typedef _di_ITTSExternalSynthesizerW _di_ITTSExternalSynthesizer;'}
{$HPPEMIT '#else'}
{$HPPEMIT 'typedef _di_IAttributesA _di_IAttributes;'}
{$HPPEMIT 'typedef _di_ILexPronounceA _di_ILexPronounce;'}
{$HPPEMIT 'typedef _di_ISRAttributesA _di_ISRAttributes;'}
{$HPPEMIT 'typedef _di_ISRCentralA _di_ISRCentral;'}
{$HPPEMIT 'typedef _di_ISRDialogsA _di_ISRDialogs;'}
{$HPPEMIT 'typedef _di_ISREnumA _di_ISREnum;'}
{$HPPEMIT 'typedef _di_ISRFindA _di_ISRFind;'}
{$HPPEMIT 'typedef _di_ISRGramCommonA _di_ISRGramCommon;'}
{$HPPEMIT 'typedef _di_ISRGramCFGA _di_ISRGramCFG;'}
{$HPPEMIT 'typedef _di_ISRGramDictationA _di_ISRGramDictation;'}
{$HPPEMIT 'typedef _di_ISRGramNotifySinkA _di_ISRGramNotifySink;'}
{$HPPEMIT 'typedef _di_ISRResBasicA _di_ISRResBasic;'}
{$HPPEMIT 'typedef _di_ISRResCorrectionA _di_ISRResCorrection;'}
{$HPPEMIT 'typedef _di_ISRResGraphA _di_ISRResGraph;'}
{$HPPEMIT 'typedef _di_ISRResSpeakerA _di_ISRResSpeaker;'}
{$HPPEMIT 'typedef _di_ISRSpeakerA _di_ISRSpeaker;'}
{$HPPEMIT 'typedef _di_ITTSAttributesA _di_ITTSAttributes;'}
{$HPPEMIT 'typedef _di_ITTSCentralA _di_ITTSCentral;'}
{$HPPEMIT 'typedef _di_ITTSDialogsA _di_ITTSDialogs;'}
{$HPPEMIT 'typedef _di_ITTSEnumA _di_ITTSEnum;'}
{$HPPEMIT 'typedef _di_ITTSFindA _di_ITTSFind;'}
{$HPPEMIT 'typedef _di_ITTSNotifySinkA _di_ITTSNotifySink;'}
{$HPPEMIT 'typedef _di_IVCmdNotifySinkA _di_IVCmdNotifySink;'}
{$HPPEMIT 'typedef _di_IVCmdEnumA _di_IVCmdEnum;'}
{$HPPEMIT 'typedef _di_IEnumSRShareA _di_IEnumSRShare;'}
{$HPPEMIT 'typedef _di_IVCmdMenuA _di_IVCmdMenu;'}
{$HPPEMIT 'typedef _di_IVoiceCmdA _di_IVoiceCmd;'}
{$HPPEMIT 'typedef _di_IVCmdAttributesA _di_IVCmdAttributes;'}
{$HPPEMIT 'typedef _di_IVCmdDialogsA _di_IVCmdDialogs;'}
{$HPPEMIT 'typedef _di_IVTxtNotifySinkA _di_IVTxtNotifySink;'}
{$HPPEMIT 'typedef _di_IVoiceTextA _di_IVoiceText;'}
{$HPPEMIT 'typedef _di_IVTxtAttributesA _di_IVTxtAttributes;'}
{$HPPEMIT 'typedef _di_IVTxtDialogsA _di_IVTxtDialogs;'}
{$HPPEMIT 'typedef _di_IVDctNotifySinkA _di_IVDctNotifySink;'}
{$HPPEMIT 'typedef _di_IVoiceDictationA _di_IVoiceDictation;'}
{$HPPEMIT 'typedef _di_IVDctTextA _di_IVDctText;'}
{$HPPEMIT 'typedef _di_IVDctInvTextNormA _di_IVDctInvTextNorm;'}
{$HPPEMIT 'typedef _di_IVDctAttributesA _di_IVDctAttributes;'}
{$HPPEMIT 'typedef _di_IVDctCommandsA _di_IVDctCommands;'}
{$HPPEMIT 'typedef _di_IVDctGlossaryA _di_IVDctGlossary;'}
{$HPPEMIT 'typedef _di_IVDctDialogsA _di_IVDctDialogs;'}
{$HPPEMIT 'typedef _di_IVDctCommandsBuiltInA _di_IVDctCommandsBuiltIn;'}
{$HPPEMIT 'typedef _di_IVDctCommandsAppA _di_IVDctCommandsApp;'}
{$HPPEMIT 'typedef _di_ISRDialogs2A _di_ISRDialogs2;'}
{$HPPEMIT 'typedef _di_ILexPronounce2A _di_ILexPronounce2;'}
{$HPPEMIT 'typedef _di_ISpchErrorA _di_ISpchError;'}
{$HPPEMIT 'typedef _di_ISRGramLexPronA _di_ISRGramLexPron;'}
{$HPPEMIT 'typedef _di_ISRSpeaker2A _di_ISRSpeaker2;'}
{$HPPEMIT 'typedef _di_ITTSNotifySink2A _di_ITTSNotifySink2;'}
{$HPPEMIT 'typedef _di_IVDctNotifySink2A _di_IVDctNotifySink2;'}
{$HPPEMIT 'typedef _di_IVDctText2A _di_IVDctText2;'}
{$HPPEMIT 'typedef _di_ITTSExternalSynthesizerA _di_ITTSExternalSynthesizer;'}
{$HPPEMIT '#endif'}

const
// CLASS IDs
  {$EXTERNALSYM CLSID_STMicWizard}
  CLSID_STMicWizard: TGUID = '{b9f11a95-90e3-11d0-8d77-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_STLexDlg}
  CLSID_STLexDlg: TGUID = '{92655fb1-adf9-11d1-beb9-006008317ce8}';
  {$EXTERNALSYM CLSID_STPhoneConv}
  CLSID_STPhoneConv: TGUID = '{b9f11a96-90e3-11d0-8d77-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_MMAudioDest}
  CLSID_MMAudioDest: TGUID = '{cb96b400-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM CLSID_MMAudioSource}
  CLSID_MMAudioSource: TGUID = '{d24fe500-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM CLSID_InstAudioSource}
  CLSID_InstAudioSource: TGUID = '{d4023720-e4b9-11cf-8d56-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_SuspAudioSource}
  CLSID_SuspAudioSource: TGUID = '{d4123720-e4b9-11cf-8d56-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_AudioSourceFile}
  CLSID_AudioSourceFile: TGUID = '{d4523720-e4b9-11cf-8d56-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_AudioDestFile}
  CLSID_AudioDestFile: TGUID = '{d4623720-e4b9-11cf-8d56-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_AudioDestDirect}
  CLSID_AudioDestDirect: TGUID = '{b9f11a90-90e3-11d0-8d77-00a0c9034a7e}';
  {$EXTERNALSYM CLSID_AudioSourceDirect}
  CLSID_AudioSourceDirect: TGUID = '{32c35401-d04f-11d0-99b3-00aa004cd65c}';
  {$EXTERNALSYM CLSID_SREnumerator}
  CLSID_SREnumerator: TGUID = '{e02d16c0-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM CLSID_TTSEnumerator}
  CLSID_TTSEnumerator: TGUID = '{d67c0280-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM CLSID_VCmd}
  CLSID_VCmd: TGUID = '{6d40d820-0ba7-11ce-a166-00aa004cd65c}';
  {$EXTERNALSYM CLSID_SRShare}
  CLSID_SRShare: TGUID = '{89f70c30-8636-11ce-b763-00aa004cd65c}';
  {$EXTERNALSYM CLSID_VDct}
  CLSID_VDct: TGUID = '{35522ca0-67ce-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM CLSID_VTxt}
  CLSID_VTxt: TGUID = '{f1dc95a0-0ba7-11ce-a166-00aa004cd65c}';
  {$EXTERNALSYM CLSID_STGramComp}
  CLSID_STGramComp: TGUID = '{70618f72-d1ed-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM CLSID_STTTSQueue}
  CLSID_STTTSQueue: TGUID = '{efd0e6ba-db5f-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM CLSID_STLog}
  CLSID_STLog: TGUID = '{b3613d9f-e26e-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM CLSID_AudioSourceLog}
  CLSID_AudioSourceLog: TGUID = '{b3613da0-e26e-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM CLSID_AudioSourceTel}
  CLSID_AudioSourceTel: TGUID = '{2ec5a8a5-e65b-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM CLSID_AudioDestTel}
  CLSID_AudioDestTel: TGUID = '{2ec5a8a6-e65b-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM CLSID_STRecord}
  CLSID_STRecord: TGUID = '{61935832-fc85-11d0-8fae-08002be4e62a}';
  {$EXTERNALSYM CLSID_SynthAudioDest}
  CLSID_SynthAudioDest: TGUID = '{479dfdcb-01a8-11d1-a7a5-00c04fbbe8ab}';
  {$EXTERNALSYM CLSID_SynthEngine}
  CLSID_SynthEngine: TGUID = '{26c328c1-f26f-11d0-9dff-006097622195}';
  {$EXTERNALSYM CLSID_SynthEngineEnumerator}
  CLSID_SynthEngineEnumerator: TGUID = '{ac8fcca1-ffd7-11d0-9e22-006097622195}';

// Interface IDs

  {$EXTERNALSYM IID_IAttributesW}
  IID_IAttributesW: TGUID = '{1d59ded1-e367-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_IAttributesA}
  IID_IAttributesA: TGUID = '{1d59ded2-e367-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISpchErrorW}
  IID_ISpchErrorW: TGUID = '{9b445335-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISpchErrorA}
  IID_ISpchErrorA: TGUID = '{9b445336-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ILexPronounceW}
  IID_ILexPronounceW: TGUID = '{090CD9A2-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ILexPronounceA}
  IID_ILexPronounceA: TGUID = '{2F26B9C0-DB31-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ILexPronounce2W}
  IID_ILexPronounce2W: TGUID = '{d6e56341-b3ea-11d1-afa5-0000f81e880d}';
  {$EXTERNALSYM IID_ILexPronounce2A}
  IID_ILexPronounce2A: TGUID = '{d6e56342-b3ea-11d1-afa5-0000f81e880d}';
  {$EXTERNALSYM IID_ISTMicWizard}
  IID_ISTMicWizard: TGUID = '{b9f11a97-90e3-11d0-8d77-00a0c9034a7e}';
  {$EXTERNALSYM IID_ISTLexDlg}
  IID_ISTLexDlg: TGUID = '{a27b8a01-adf9-11d1-beb9-006008317ce8}';
  {$EXTERNALSYM IID_ISTPhoneConv}
  IID_ISTPhoneConv: TGUID = '{b9f11a98-90e3-11d0-8d77-00a0c9034a7e}';
  {$EXTERNALSYM IID_IAudio}
  IID_IAudio: TGUID = '{f546b340-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_IAudioDest}
  IID_IAudioDest: TGUID = '{2ec34da0-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_IAudioDestNotifySink}
  IID_IAudioDestNotifySink: TGUID = '{acb08c00-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_IAudioMultiMediaDevice}
  IID_IAudioMultiMediaDevice: TGUID = '{b68ad320-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_IAudioSource}
  IID_IAudioSource: TGUID = '{bc06a220-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_IAudioSourceInstrumented}
  IID_IAudioSourceInstrumented: TGUID = '{d4023721-e4b9-11cf-8d56-00a0c9034a7e}';
  {$EXTERNALSYM IID_IAudioSourceSusp}
  IID_IAudioSourceSusp: TGUID = '{d4123721-e4b9-11cf-8d56-00a0c9034a7e}';
  {$EXTERNALSYM IID_IAudioSourceNotifySink}
  IID_IAudioSourceNotifySink: TGUID = '{c0bd9a80-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_IAudioFileNotifySink}
  IID_IAudioFileNotifySink: TGUID = '{492fe490-51e7-11b9-c000-fed6cba3b1a9}';
  {$EXTERNALSYM IID_IAudioFile}
  IID_IAudioFile: TGUID = '{fd7c2320-3d6d-11b9-c000-fed6cba3b1a9}';
  {$EXTERNALSYM IID_IAudioDirect}
  IID_IAudioDirect: TGUID = '{b9f11a94-90e3-11d0-8d77-00a0c9034a7e}';
  {$EXTERNALSYM IID_ISRAttributesW}
  IID_ISRAttributesW: TGUID = '{68A33AA0-44CD-101B-90A8-00AA003E4B50}';
  {$EXTERNALSYM IID_ISRAttributesA}
  IID_ISRAttributesA: TGUID = '{2F26B9C1-DB31-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRCentralW}
  IID_ISRCentralW: TGUID = '{B9BD3860-44DB-101B-90A8-00AA003E4B50}';
  {$EXTERNALSYM IID_ISRCentralA}
  IID_ISRCentralA: TGUID = '{2F26B9C2-DB31-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRDialogsW}
  IID_ISRDialogsW: TGUID = '{BCFB4C60-44DB-101B-90A8-00AA003E4B50}';
  {$EXTERNALSYM IID_ISRDialogsA}
  IID_ISRDialogsA: TGUID = '{05EB6C60-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRDialogs2W}
  IID_ISRDialogs2W: TGUID = '{9b445330-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISRDialogs2A}
  IID_ISRDialogs2A: TGUID = '{9b445331-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISREnumW}
  IID_ISREnumW: TGUID = '{BFA9F1A0-44DB-101B-90A8-00AA003E4B50}';
  {$EXTERNALSYM IID_ISREnumA}
  IID_ISREnumA: TGUID = '{05EB6C61-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRFindW}
  IID_ISRFindW: TGUID = '{C2835060-44DB-101B-90A8-00AA003E4B50}';
  {$EXTERNALSYM IID_ISRFindA}
  IID_ISRFindA: TGUID = '{05EB6C62-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramLexPronW}
  IID_ISRGramLexPronW: TGUID = '{7eb4b631-19a7-11d1-af95-0000f81e880d}';
  {$EXTERNALSYM IID_ISRGramLexPronA}
  IID_ISRGramLexPronA: TGUID = '{7eb4b632-19a7-11d1-af95-0000f81e880d}';
  {$EXTERNALSYM IID_ISRGramCommonW}
  IID_ISRGramCommonW: TGUID = '{e8c3e160-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_ISRGramCommonA}
  IID_ISRGramCommonA: TGUID = '{05EB6C63-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramCFGW}
  IID_ISRGramCFGW: TGUID = '{ecc0b180-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_ISRGramCFGA}
  IID_ISRGramCFGA: TGUID = '{05EB6C64-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramDictationW}
  IID_ISRGramDictationW: TGUID = '{090CD9A3-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramDictationA}
  IID_ISRGramDictationA: TGUID = '{05EB6C65-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramInsertionGUI}
  IID_ISRGramInsertionGUI: TGUID = '{090CD9A4-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramNotifySinkW}
  IID_ISRGramNotifySinkW: TGUID = '{f106bfa0-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_ISRGramNotifySinkA}
  IID_ISRGramNotifySinkA: TGUID = '{efeea350-ce5e-11cd-9d96-00aa002fc7c9}';
  {$EXTERNALSYM IID_ISRNotifySink}
  IID_ISRNotifySink: TGUID = '{090CD9B0-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRNotifySink2}
  IID_ISRNotifySink2: TGUID = '{9b445332-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISRResBasicW}
  IID_ISRResBasicW: TGUID = '{090CD9A5-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResBasicA}
  IID_ISRResBasicA: TGUID = '{05EB6C66-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResScores}
  IID_ISRResScores: TGUID = '{0b37f1e0-b8de-11cf-b22e-00aa00a215ed}';
  {$EXTERNALSYM IID_ISRResMerge}
  IID_ISRResMerge: TGUID = '{090CD9A6-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResAudio}
  IID_ISRResAudio: TGUID = '{090CD9A7-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResAudioEx}
  IID_ISRResAudioEx: TGUID = '{aad74c30-4b02-11d1-b792-0060979841b4}';
  {$EXTERNALSYM IID_ISRResCorrectionW}
  IID_ISRResCorrectionW: TGUID = '{090CD9A8-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResCorrectionA}
  IID_ISRResCorrectionA: TGUID = '{05EB6C67-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResEval}
  IID_ISRResEval: TGUID = '{090CD9A9-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResGraphW}
  IID_ISRResGraphW: TGUID = '{090CD9AA-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResGraphA}
  IID_ISRResGraphA: TGUID = '{05EB6C68-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResGraphEx}
  IID_ISRResGraphEx: TGUID = '{be8f63a0-b915-11d1-b7bc-0060979841b4}';
  {$EXTERNALSYM IID_ISRResMemory}
  IID_ISRResMemory: TGUID = '{090CD9AB-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResModifyGUI}
  IID_ISRResModifyGUI: TGUID = '{090CD9AC-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResSpeakerW}
  IID_ISRResSpeakerW: TGUID = '{090CD9AD-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResSpeakerA}
  IID_ISRResSpeakerA: TGUID = '{05EB6C69-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRSpeakerW}
  IID_ISRSpeakerW: TGUID = '{090CD9AE-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRSpeakerA}
  IID_ISRSpeakerA: TGUID = '{090CD9AF-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRSpeaker2W}
  IID_ISRSpeaker2W: TGUID = '{9b445333-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISRSpeaker2A}
  IID_ISRSpeaker2A: TGUID = '{9b445334-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ITTSAttributesW}
  IID_ITTSAttributesW: TGUID = '{1287A280-4A47-101B-931A-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSAttributesA}
  IID_ITTSAttributesA: TGUID = '{0FD6E2A1-E77D-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSBufNotifySink}
  IID_ITTSBufNotifySink: TGUID = '{e4963d40-c743-11cd-80e5-00aa003e4b50}';
  {$EXTERNALSYM IID_ITTSCentralW}
  IID_ITTSCentralW: TGUID = '{28016060-4A47-101B-931A-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSCentralA}
  IID_ITTSCentralA: TGUID = '{05EB6C6A-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSDialogsW}
  IID_ITTSDialogsW: TGUID = '{47F59D00-4A47-101B-931A-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSDialogsA}
  IID_ITTSDialogsA: TGUID = '{05EB6C6B-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSEnumW}
  IID_ITTSEnumW: TGUID = '{6B837B20-4A47-101B-931A-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSEnumA}
  IID_ITTSEnumA: TGUID = '{05EB6C6D-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSFindW}
  IID_ITTSFindW: TGUID = '{7AA42960-4A47-101B-931A-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSFindA}
  IID_ITTSFindA: TGUID = '{05EB6C6E-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSNotifySinkW}
  IID_ITTSNotifySinkW: TGUID = '{C0FA8F40-4A46-101B-931A-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSNotifySinkA}
  IID_ITTSNotifySinkA: TGUID = '{05EB6C6F-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSNotifySink2W}
  IID_ITTSNotifySink2W: TGUID = '{599f77e1-e42e-11d1-bed8-006008317ce8}';
  {$EXTERNALSYM IID_ITTSNotifySink2A}
  IID_ITTSNotifySink2A: TGUID = '{599f77e2-e42e-11d1-bed8-006008317ce8}';
  {$EXTERNALSYM IID_IVCmdNotifySinkW}
  IID_IVCmdNotifySinkW: TGUID = '{CCFD7A60-604D-101B-9926-00AA003CFC2C}';
  {$EXTERNALSYM IID_IVCmdNotifySinkA}
  IID_IVCmdNotifySinkA: TGUID = '{80b25cc0-5540-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVCmdEnumW}
  IID_IVCmdEnumW: TGUID = '{D3CC0820-604D-101B-9926-00AA003CFC2C}';
  {$EXTERNALSYM IID_IVCmdEnumA}
  IID_IVCmdEnumA: TGUID = '{E86F9540-DCA2-11CD-A166-00AA004CD65C}';
  {$EXTERNALSYM IID_IEnumSRShareW}
  IID_IEnumSRShareW: TGUID = '{e97f05c0-81b3-11ce-b763-00aa004cd65c}';
  {$EXTERNALSYM IID_IEnumSRShareA}
  IID_IEnumSRShareA: TGUID = '{e97f05c1-81b3-11ce-b763-00aa004cd65c}';
  {$EXTERNALSYM IID_IVCmdMenuW}
  IID_IVCmdMenuW: TGUID = '{DAC54F60-604D-101B-9926-00AA003CFC2C}';
  {$EXTERNALSYM IID_IVCmdMenuA}
  IID_IVCmdMenuA: TGUID = '{746141e0-5543-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVoiceCmdW}
  IID_IVoiceCmdW: TGUID = '{E0DCC220-604D-101B-9926-00AA003CFC2C}';
  {$EXTERNALSYM IID_IVoiceCmdA}
  IID_IVoiceCmdA: TGUID = '{c63a2b30-5543-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVCmdAttributesW}
  IID_IVCmdAttributesW: TGUID = '{E5F24680-6053-101B-9926-00AA003CFC2C}';
  {$EXTERNALSYM IID_IVCmdAttributesA}
  IID_IVCmdAttributesA: TGUID = '{fff5df80-5544-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVCmdDialogsW}
  IID_IVCmdDialogsW: TGUID = '{EE39B8A0-6053-101B-9926-00AA003CFC2C}';
  {$EXTERNALSYM IID_IVCmdDialogsA}
  IID_IVCmdDialogsA: TGUID = '{aa8fe260-5545-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVDctNotifySinkW}
  IID_IVDctNotifySinkW: TGUID = '{5feb8800-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctNotifySinkA}
  IID_IVDctNotifySinkA: TGUID = '{88ad7dc0-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctNotifySink2W}
  IID_IVDctNotifySink2W: TGUID = '{599f77e3-e42e-11d1-bed8-006008317ce8}';
  {$EXTERNALSYM IID_IVDctNotifySink2A}
  IID_IVDctNotifySink2A: TGUID = '{599f77e4-e42e-11d1-bed8-006008317ce8}';
  {$EXTERNALSYM IID_IVoiceDictationW}
  IID_IVoiceDictationW: TGUID = '{88ad7dc3-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVoiceDictationA}
  IID_IVoiceDictationA: TGUID = '{88ad7dc4-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctTextW}
  IID_IVDctTextW: TGUID = '{6d62b3a0-6893-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctTextA}
  IID_IVDctTextA: TGUID = '{6d62b3a1-6893-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctText2W}
  IID_IVDctText2W: TGUID = '{238004e1-f0c4-11d1-bed9-006008317ce8}';
  {$EXTERNALSYM IID_IVDctText2A}
  IID_IVDctText2A: TGUID = '{238004e2-f0c4-11d1-bed9-006008317ce8}';
  {$EXTERNALSYM IID_IVDctTextCache}
  IID_IVDctTextCache: TGUID = '{238004e3-f0c4-11d1-bed9-006008317ce8}';
  {$EXTERNALSYM IID_IVDctInvTextNormW}
  IID_IVDctInvTextNormW: TGUID = '{90a84ea0-6e51-11d0-9bc2-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctInvTextNormA}
  IID_IVDctInvTextNormA: TGUID = '{90a84ea1-6e51-11d0-9bc2-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctAttributesW}
  IID_IVDctAttributesW: TGUID = '{88ad7dc5-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctAttributesA}
  IID_IVDctAttributesA: TGUID = '{88ad7dc6-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctCommandsBuiltInW}
  IID_IVDctCommandsBuiltInW: TGUID = '{d4e3f8e0-6521-11d1-8c35-006097df5d01}';
  {$EXTERNALSYM IID_IVDctCommandsBuiltInA}
  IID_IVDctCommandsBuiltInA: TGUID = '{8763afd1-7ade-11d1-bea7-006008317ce8}';
  {$EXTERNALSYM IID_IVDctCommandsAppW}
  IID_IVDctCommandsAppW: TGUID = '{d4e3f8e1-6521-11d1-8c35-006097df5d01}';
  {$EXTERNALSYM IID_IVDctCommandsAppA}
  IID_IVDctCommandsAppA: TGUID = '{8763afd2-7ade-11d1-bea7-006008317ce8}';
  {$EXTERNALSYM IID_IVDctCommandsW}
  IID_IVDctCommandsW: TGUID = '{A02C2CA0-AE50-11cf-833A-00AA00A21A29}';
  {$EXTERNALSYM IID_IVDctCommandsA}
  IID_IVDctCommandsA: TGUID = '{A02C2CA1-AE50-11cf-833A-00AA00A21A29}';
  {$EXTERNALSYM IID_IVDctGlossaryW}
  IID_IVDctGlossaryW: TGUID = '{A02C2CA2-AE50-11cf-833A-00AA00A21A29}';
  {$EXTERNALSYM IID_IVDctGlossaryA}
  IID_IVDctGlossaryA: TGUID = '{A02C2CA3-AE50-11cf-833A-00AA00A21A29}';
  {$EXTERNALSYM IID_IVDctDialogsW}
  IID_IVDctDialogsW: TGUID = '{88ad7dc7-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctDialogsA}
  IID_IVDctDialogsA: TGUID = '{88ad7dc8-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctGUI}
  IID_IVDctGUI: TGUID = '{8953f1a0-7e80-11cf-8d15-00a0c9034a7e}';
  {$EXTERNALSYM IID_IVTxtNotifySinkW}
  IID_IVTxtNotifySinkW: TGUID = '{fd3a2430-e090-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtNotifySinkA}
  IID_IVTxtNotifySinkA: TGUID = '{d2c840e0-e092-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVoiceTextW}
  IID_IVoiceTextW: TGUID = '{c4fe8740-e093-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVoiceTextA}
  IID_IVoiceTextA: TGUID = '{e1b7a180-e093-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtAttributesW}
  IID_IVTxtAttributesW: TGUID = '{6a8d6140-e095-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtAttributesA}
  IID_IVTxtAttributesA: TGUID = '{8be9cc30-e095-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtDialogsW}
  IID_IVTxtDialogsW: TGUID = '{d6469210-e095-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtDialogsA}
  IID_IVTxtDialogsA: TGUID = '{e8f6fa20-e095-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_ISTRecord}
  IID_ISTRecord: TGUID = '{61935833-fc85-11d0-8fae-08002be4e62a}';
  {$EXTERNALSYM IID_ISTRecordNotifySink}
  IID_ISTRecordNotifySink: TGUID = '{61935834-fc85-11d0-8fae-08002be4e62a}';
  {$EXTERNALSYM IID_ISTGramComp}
  IID_ISTGramComp: TGUID = '{70618f73-d1ed-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM IID_ISTTTSQueue}
  IID_ISTTTSQueue: TGUID = '{efd0e6bb-db5f-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM IID_ISTTTSQueueNotifySink}
  IID_ISTTTSQueueNotifySink: TGUID = '{8412fa0c-db6a-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM IID_ISTLog}
  IID_ISTLog: TGUID = '{b3613da1-e26e-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM IID_IAudioSourceLog}
  IID_IAudioSourceLog: TGUID = '{b3613da2-e26e-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM IID_IAudioTel}
  IID_IAudioTel: TGUID = '{2ec5a8a7-e65b-11d0-8fac-08002be4e62a}';
  {$EXTERNALSYM IID_ITTSExternalSynthesizerW}
  IID_ITTSExternalSynthesizerW: TGUID = '{a3f8b9c0-2010-11d1-a306-006097622195}';
  {$EXTERNALSYM IID_ITTSExternalSynthesizerA}
  IID_ITTSExternalSynthesizerA: TGUID = '{3d11c963-f210-11d0-a788-00c04fbbe8ab}';
  {$EXTERNALSYM IID_IAudioExternalSynthesizer}
  IID_IAudioExternalSynthesizer: TGUID = '{33e0e76b-70ce-11d1-a826-00c04fbbe8ab}';
  
  // The following is hardcoded in the par file to ANSI because I couldn't figure 
  // out how with {#BEGIN} {$} {#END} and the SY238 bug.
  {$EXTERNALSYM IID_IAttributes}
  IID_IAttributes: TGUID = '{1d59ded2-e367-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISpchError}
  IID_ISpchError: TGUID = '{9b445336-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ILexPronounce}
  IID_ILexPronounce: TGUID = '{2F26B9C0-DB31-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ILexPronounce2}
  IID_ILexPronounce2: TGUID = '{d6e56342-b3ea-11d1-afa5-0000f81e880d}';
  {$EXTERNALSYM IID_ISRAttributes}
  IID_ISRAttributes: TGUID = '{2F26B9C1-DB31-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRCentral}
  IID_ISRCentral: TGUID = '{2F26B9C2-DB31-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRDialogs}
  IID_ISRDialogs: TGUID = '{05EB6C60-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRDialogs2}
  IID_ISRDialogs2: TGUID = '{9b445331-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ISREnum}
  IID_ISREnum: TGUID = '{05EB6C61-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRFind}
  IID_ISRFind: TGUID = '{05EB6C62-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramLexPron}
  IID_ISRGramLexPron: TGUID = '{7eb4b632-19a7-11d1-af95-0000f81e880d}';
  {$EXTERNALSYM IID_ISRGramCommon}
  IID_ISRGramCommon: TGUID = '{05EB6C63-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramCFG}
  IID_ISRGramCFG: TGUID = '{05EB6C64-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramDictation}
  IID_ISRGramDictation: TGUID = '{05EB6C65-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRGramNotifySink}
  IID_ISRGramNotifySink: TGUID = '{efeea350-ce5e-11cd-9d96-00aa002fc7c9}';
  {$EXTERNALSYM IID_ISRResBasic}
  IID_ISRResBasic: TGUID = '{05EB6C66-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResCorrection}
  IID_ISRResCorrection: TGUID = '{05EB6C67-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResGraph}
  IID_ISRResGraph: TGUID = '{05EB6C68-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRResSpeaker}
  IID_ISRResSpeaker: TGUID = '{05EB6C69-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRSpeaker}
  IID_ISRSpeaker: TGUID = '{090CD9AF-DA1A-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ISRSpeaker2}
  IID_ISRSpeaker2: TGUID = '{9b445334-e39f-11d1-bed7-006008317ce8}';
  {$EXTERNALSYM IID_ITTSAttributes}
  IID_ITTSAttributes: TGUID = '{0FD6E2A1-E77D-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSCentral}
  IID_ITTSCentral: TGUID = '{05EB6C6A-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSDialogs}
  IID_ITTSDialogs: TGUID = '{05EB6C6B-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSEnum}
  IID_ITTSEnum: TGUID = '{05EB6C6D-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSFind}
  IID_ITTSFind: TGUID = '{05EB6C6E-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSNotifySink}
  IID_ITTSNotifySink: TGUID = '{05EB6C6F-DBAB-11CD-B3CA-00AA0047BA4F}';
  {$EXTERNALSYM IID_ITTSNotifySink2}
  IID_ITTSNotifySink2: TGUID = '{599f77e2-e42e-11d1-bed8-006008317ce8}';
  {$EXTERNALSYM IID_IVCmdNotifySink}
  IID_IVCmdNotifySink: TGUID = '{80b25cc0-5540-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVCmdEnum}
  IID_IVCmdEnum: TGUID = '{E86F9540-DCA2-11CD-A166-00AA004CD65C}';
  {$EXTERNALSYM IID_IEnumSRShare}
  IID_IEnumSRShare: TGUID = '{e97f05c1-81b3-11ce-b763-00aa004cd65c}';
  {$EXTERNALSYM IID_IVCmdMenu}
  IID_IVCmdMenu: TGUID = '{746141e0-5543-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVoiceCmd}
  IID_IVoiceCmd: TGUID = '{c63a2b30-5543-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVCmdAttributes}
  IID_IVCmdAttributes: TGUID = '{fff5df80-5544-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVCmdDialogs}
  IID_IVCmdDialogs: TGUID = '{aa8fe260-5545-11b9-c000-5611722e1d15}';
  {$EXTERNALSYM IID_IVDctNotifySink}
  IID_IVDctNotifySink: TGUID = '{88ad7dc0-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctNotifySink2}
  IID_IVDctNotifySink2: TGUID = '{599f77e4-e42e-11d1-bed8-006008317ce8}';
  {$EXTERNALSYM IID_IVoiceDictation}
  IID_IVoiceDictation: TGUID = '{88ad7dc4-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctText}
  IID_IVDctText: TGUID = '{6d62b3a1-6893-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctText2}
  IID_IVDctText2: TGUID = '{238004e2-f0c4-11d1-bed9-006008317ce8}';
  {$EXTERNALSYM IID_IVDctInvTextNorm}
  IID_IVDctInvTextNorm: TGUID = '{90a84ea1-6e51-11d0-9bc2-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctAttributes}
  IID_IVDctAttributes: TGUID = '{88ad7dc6-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVDctCommandsBuiltIn}
  IID_IVDctCommandsBuiltIn: TGUID = '{8763afd1-7ade-11d1-bea7-006008317ce8}';
  {$EXTERNALSYM IID_IVDctCommandsApp}
  IID_IVDctCommandsApp: TGUID = '{8763afd2-7ade-11d1-bea7-006008317ce8}';
  {$EXTERNALSYM IID_IVDctCommands}
  IID_IVDctCommands: TGUID = '{A02C2CA1-AE50-11cf-833A-00AA00A21A29}';
  {$EXTERNALSYM IID_IVDctGlossary}
  IID_IVDctGlossary: TGUID = '{A02C2CA3-AE50-11cf-833A-00AA00A21A29}';
  {$EXTERNALSYM IID_IVDctDialogs}
  IID_IVDctDialogs: TGUID = '{88ad7dc8-67d5-11cf-9b8b-08005afc3a41}';
  {$EXTERNALSYM IID_IVTxtNotifySink}
  IID_IVTxtNotifySink: TGUID = '{d2c840e0-e092-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVoiceText}
  IID_IVoiceText: TGUID = '{e1b7a180-e093-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtAttributes}
  IID_IVTxtAttributes: TGUID = '{8be9cc30-e095-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_IVTxtDialogs}
  IID_IVTxtDialogs: TGUID = '{e8f6fa20-e095-11cd-a166-00aa004cd65c}';
  {$EXTERNALSYM IID_ITTSExternalSynthesizer}
  IID_ITTSExternalSynthesizer: TGUID = '{3d11c963-f210-11d0-a788-00c04fbbe8ab}';

  
// String constants for Interface IDs
  // Wide IDs
  SID_IAttributesW             = '{1d59ded1-e367-11d1-bed7-006008317ce8}'; //SAPI 4.0
  SID_ILexPronounceW           = '{090CD9A2-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRAttributesW           = '{68A33AA0-44CD-101B-90A8-00AA003E4B50}';
  SID_ISRCentralW              = '{B9BD3860-44DB-101B-90A8-00AA003E4B50}';
  SID_ISRDialogsW              = '{BCFB4C60-44DB-101B-90A8-00AA003E4B50}';
  SID_ISREnumW                 = '{BFA9F1A0-44DB-101B-90A8-00AA003E4B50}';
  SID_ISRFindW                 = '{C2835060-44DB-101B-90A8-00AA003E4B50}';
  SID_ISRGramCommonW           = '{e8c3e160-c743-11cd-80e5-00aa003e4b50}';
  SID_ISRGramCFGW              = '{ecc0b180-c743-11cd-80e5-00aa003e4b50}';
  SID_ISRGramDictationW        = '{090CD9A3-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRGramNotifySinkW       = '{f106bfa0-c743-11cd-80e5-00aa003e4b50}';
  SID_ISRResBasicW             = '{090CD9A5-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResCorrectionW        = '{090CD9A8-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResGraphW             = '{090CD9AA-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResSpeakerW           = '{090CD9AD-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRSpeakerW              = '{090CD9AE-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSAttributesW          = '{1287A280-4A47-101B-931A-00AA0047BA4F}';
  SID_ITTSCentralW             = '{28016060-4A47-101B-931A-00AA0047BA4F}';
  SID_ITTSDialogsW             = '{47F59D00-4A47-101B-931A-00AA0047BA4F}';
  SID_ITTSEnumW                = '{6B837B20-4A47-101B-931A-00AA0047BA4F}';
  SID_ITTSFindW                = '{7AA42960-4A47-101B-931A-00AA0047BA4F}';
  SID_ITTSNotifySinkW          = '{C0FA8F40-4A46-101B-931A-00AA0047BA4F}';
  SID_IVCmdNotifySinkW         = '{CCFD7A60-604D-101B-9926-00AA003CFC2C}';
  SID_IVCmdEnumW               = '{D3CC0820-604D-101B-9926-00AA003CFC2C}';
  SID_IEnumSRShareW            = '{e97f05c0-81b3-11ce-b763-00aa004cd65c}';
  SID_IVCmdMenuW               = '{DAC54F60-604D-101B-9926-00AA003CFC2C}';
  SID_IVoiceCmdW               = '{E0DCC220-604D-101B-9926-00AA003CFC2C}';
  SID_IVCmdAttributesW         = '{E5F24680-6053-101B-9926-00AA003CFC2C}';
  SID_IVCmdDialogsW            = '{EE39B8A0-6053-101B-9926-00AA003CFC2C}';
  SID_IVTxtNotifySinkW         = '{fd3a2430-e090-11cd-a166-00aa004cd65c}';
  SID_IVoiceTextW              = '{c4fe8740-e093-11cd-a166-00aa004cd65c}';
  SID_IVTxtAttributesW         = '{6a8d6140-e095-11cd-a166-00aa004cd65c}';
  SID_IVTxtDialogsW            = '{d6469210-e095-11cd-a166-00aa004cd65c}';
  SID_IVDctNotifySinkW         = '{5FEB8800-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVoiceDictationW         = '{88AD7DC3-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctTextW               = '{6D62B3A0-6893-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctInvTextNormW        = '{90A84EA0-6E51-11d0-9BC2-08005AFC3A41}'; {SAPI 3}
  SID_IVDctAttributesW         = '{88AD7DC5-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctCommandsW           = '{A02C2CA0-AE50-11cf-833A-00AA00A21A29}'; {SAPI 3}
  SID_IVDctGlossaryW           = '{A02C2CA2-AE50-11cf-833A-00AA00A21A29}'; {SAPI 3}
  SID_IVDctDialogsW            = '{88AD7DC7-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctCommandsBuiltInW    = '{D4E3F8E0-6521-11d1-8C35-006097DF5D01}'; {SAPI 4}
  SID_IVDctCommandsAppW        = '{D4E3F8E1-6521-11d1-8C35-006097DF5D01}'; {SAPI 4}
  SID_ISpchErrorW              = '{9b445335-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ILexPronounce2W          = '{d6e56341-b3ea-11d1-afa5-0000f81e880d}'; //SAPI 4
  SID_ISRDialogs2W             = '{9b445330-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ISRGramLexPronW          = '{7eb4b631-19a7-11d1-af95-0000f81e880d}'; //SAPI 4
  SID_ISRSpeaker2W             = '{9b445333-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ITTSNotifySink2W         = '{599f77e1-e42e-11d1-bed8-006008317ce8}'; //SAPI 4
  SID_IVDctNotifySink2W        = '{599f77e3-e42e-11d1-bed8-006008317ce8}'; //SAPI 4
  SID_IVDctText2W              = '{238004e1-f0c4-11d1-bed9-006008317ce8}'; //SAPI 4
  SID_ITTSExternalSynthesizerW = '{a3f8b9c0-2010-11d1-a306-006097622195}'; //SAPI 4

  // ANSI IIDs
  SID_IAttributesA             = '{1d59ded2-e367-11d1-bed7-006008317ce8}'; //SAPI 4.0
  SID_ILexPronounceA           = '{2F26B9C0-DB31-11CD-B3CA-00AA0047BA4F}';
  SID_ISRAttributesA           = '{2F26B9C1-DB31-11CD-B3CA-00AA0047BA4F}';
  SID_ISRCentralA              = '{2F26B9C2-DB31-11CD-B3CA-00AA0047BA4F}';
  SID_ISRDialogsA              = '{05EB6C60-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISREnumA                 = '{05EB6C61-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRFindA                 = '{05EB6C62-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRGramCommonA           = '{05EB6C63-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRGramCFGA              = '{05EB6C64-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRGramDictationA        = '{05EB6C65-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRGramNotifySinkA       = '{efeea350-ce5e-11cd-9d96-00aa002fc7c9}';
  SID_ISRResBasicA             = '{05EB6C66-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResCorrectionA        = '{05EB6C67-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResGraphA             = '{05EB6C68-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResSpeakerA           = '{05EB6C69-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ISRSpeakerA              = '{090CD9AF-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSAttributesA          = '{0FD6E2A1-E77D-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSCentralA             = '{05EB6C6A-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSDialogsA             = '{05EB6C6B-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSEnumA                = '{05EB6C6D-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSFindA                = '{05EB6C6E-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSNotifySinkA          = '{05EB6C6F-DBAB-11CD-B3CA-00AA0047BA4F}';
  SID_IVCmdNotifySinkA         = '{80b25cc0-5540-11b9-c000-5611722e1d15}';
  SID_IVCmdEnumA               = '{E86F9540-DCA2-11CD-A166-00AA004CD65C}';
  SID_IEnumSRShareA            = '{e97f05c1-81b3-11ce-b763-00aa004cd65c}';
  SID_IVCmdMenuA               = '{746141e0-5543-11b9-c000-5611722e1d15}';
  SID_IVoiceCmdA               = '{c63a2b30-5543-11b9-c000-5611722e1d15}';
  SID_IVCmdAttributesA         = '{fff5df80-5544-11b9-c000-5611722e1d15}';
  SID_IVCmdDialogsA            = '{aa8fe260-5545-11b9-c000-5611722e1d15}';
  SID_IVTxtNotifySinkA         = '{d2c840e0-e092-11cd-a166-00aa004cd65c}';
  SID_IVoiceTextA              = '{e1b7a180-e093-11cd-a166-00aa004cd65c}';
  SID_IVTxtAttributesA         = '{8be9cc30-e095-11cd-a166-00aa004cd65c}';
  SID_IVTxtDialogsA            = '{e8f6fa20-e095-11cd-a166-00aa004cd65c}';
  SID_IVDctNotifySinkA         = '{88ad7dc0-67d5-11cf-9b8b-08005afc3a41}'; {SAPI 3}
  SID_IVoiceDictationA         = '{88AD7DC4-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctTextA               = '{6D62B3A1-6893-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctInvTextNormA        = '{90A84EA1-6E51-11d0-9BC2-08005AFC3A41}'; {SAPI 3}
  SID_IVDctAttributesA         = '{88AD7DC6-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctCommandsA           = '{A02C2CA1-AE50-11cf-833A-00AA00A21A29}'; {SAPI 3}
  SID_IVDctGlossaryA           = '{A02C2CA3-AE50-11cf-833A-00AA00A21A29}'; {SAPI 3}
  SID_IVDctDialogsA            = '{88AD7DC8-67D5-11cf-9B8B-08005AFC3A41}'; {SAPI 3}
  SID_IVDctCommandsBuiltInA    = '{8763AFD1-7ADE-11d1-BEA7-006008317CE8}'; {SAPI 4}
  SID_IVDctCommandsAppA        = '{8763AFD2-7ADE-11d1-BEA7-006008317CE8}'; {SAPI 4}
  SID_ISRDialogs2A             = '{9b445331-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ILexPronounce2A          = '{d6e56342-b3ea-11d1-afa5-0000f81e880d}'; //SAPI 4
  SID_ISpchErrorA              = '{9b445336-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ISRGramLexPronA          = '{7eb4b632-19a7-11d1-af95-0000f81e880d}'; //SAPI 4
  SID_ISRSpeaker2A             = '{9b445334-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ITTSNotifySink2A         = '{599f77e2-e42e-11d1-bed8-006008317ce8}'; //SAPI 4
  SID_IVDctNotifySink2A        = '{599f77e4-e42e-11d1-bed8-006008317ce8}'; //SAPI 4
  SID_IVDctText2A              = '{238004e2-f0c4-11d1-bed9-006008317ce8}'; //SAPI 4
  SID_ITTSExternalSynthesizerA = '{3d11c963-f210-11d0-a788-00c04fbbe8ab}'; //SAPI 4

  // General IDs
  SID_IAudio                 = '{F546B340-C743-11cd-80E5-00AA003E4B50}';
  SID_IAudioDest             = '{2EC34DA0-C743-11cd-80E5-00AA003E4B50}';
  SID_IAudioDestNotifySink   = '{ACB08C00-C743-11cd-80E5-00AA003E4B50}';
  SID_IAudioMultiMediaDevice = '{B68AD320-C743-11cd-80E5-00AA003E4B50}';
  SID_IAudioSource           = '{BC06A220-C743-11cd-80E5-00AA003E4B50}';
  SID_IAudioSourceInstrumented = '{D4023721-E4B9-11cf-8D56-00A0C9034A7E}'; {SAPI 3}
  SID_IAudioSourceSusp       = '{D4123721-E4B9-11cf-8D56-00A0C9034A7E}'; {SAPI 3}
  SID_IAudioSourceNotifySink = '{C0BD9A80-C743-11cd-80E5-00AA003E4B50}';
  SID_IAudioFileNotifySink   = '{492FE490-51E7-11b9-C000-FED6CBA3B1A9}'; {SAPI 3}
  SID_IAudioFile             = '{FD7C2320-3D6D-11b9-C000-FED6CBA3B1A9}'; {SAPI 3}
  SID_IAudioDirect           = '{B9F11A94-90E3-11d0-8D77-00A0C9034A7E}'; {SAPI 3}
  SID_ISRGramInsertionGUI    = '{090CD9A4-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRNotifySink          = '{090CD9B0-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResScores           = '{0B37F1E0-B8DE-11cf-B22E-00AA00A215ED}'; {SAPI 3}
  SID_ISRResMerge            = '{090CD9A6-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResAudio            = '{090CD9A7-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResEval             = '{090CD9A9-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResMemory           = '{090CD9AB-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ISRResModifyGUI        = '{090CD9AC-DA1A-11CD-B3CA-00AA0047BA4F}';
  SID_ITTSBufNotifySink      = '{E4963D40-C743-11cd-80E5-00AA003E4B50}';
  SID_IVDctGUI               = '{8953F1A0-7E80-11cf-8D15-00A0C9034A7E}'; {SAPI 3}
  SID_ISTMicWizard           = '{B9F11A97-90E3-11d0-8D77-00A0C9034A7E}'; {SAPI 3}
  SID_ISTPhoneConv           = '{B9F11A98-90E3-11d0-8D77-00A0C9034A7E}'; {SAPI 3}
  SID_ISTGramComp            = '{70618f73-d1ed-11d0-8fac-08002be4e62a}'; //SAPI 4.0
  SID_IVDctTextCache         = '{238004E3-F0C4-11d1-BED9-006008317CE8}'; //SAPI 4.0
  SID_ISTLexDlg              = '{a27b8a01-adf9-11d1-beb9-006008317ce8}'; //SAPI 4
  SID_ISRNotifySink2         = '{9b445332-e39f-11d1-bed7-006008317ce8}'; //SAPI 4
  SID_ISRResAudioEx          = '{aad74c30-4b02-11d1-b792-0060979841b4}'; //SAPI 4
  SID_ISRResGraphEx          = '{be8f63a0-b915-11d1-b7bc-0060979841b4}'; //SAPI 4
  SID_ISTRecord              = '{61935833-fc85-11d0-8fae-08002be4e62a}'; //SAPI 4
  SID_ISTRecordNotifySink    = '{61935834-fc85-11d0-8fae-08002be4e62a}'; //SAPI 4
  SID_ISTTTSQueue            = '{efd0e6bb-db5f-11d0-8fac-08002be4e62a}'; //SAPI 4
  SID_ISTTTSQueueNotifySink  = '{8412fa0c-db6a-11d0-8fac-08002be4e62a}'; //SAPI 4
  SID_ISTLog                 = '{b3613da1-e26e-11d0-8fac-08002be4e62a}'; //SAPI 4
  SID_IAudioSourceLog        = '{b3613da2-e26e-11d0-8fac-08002be4e62a}'; //SAPI 4
  SID_IAudioTel              = '{2ec5a8a7-e65b-11d0-8fac-08002be4e62a}'; //SAPI 4
  SID_IAudioExternalSynthesizer = '{33e0e76b-70ce-11d1-a826-00c04fbbe8ab}'; //SAPI 4


  {$EXTERNALSYM SVFN_LEN}
  SVFN_LEN     = 262;
  {$EXTERNALSYM LANG_LEN}
  LANG_LEN     = 64;
  {$EXTERNALSYM EI_TITLESIZE}
  EI_TITLESIZE = 128;
  {$EXTERNALSYM EI_DESCSIZE}
  EI_DESCSIZE  = 512;
  {$EXTERNALSYM EI_FIXSIZE}
  EI_FIXSIZE   = 512;
  {$EXTERNALSYM SVPI_MFGLEN}
  SVPI_MFGLEN  = 64;
  {$EXTERNALSYM SVPI_PRODLEN}
  SVPI_PRODLEN = 64;
  {$EXTERNALSYM SVPI_COMPLEN}
  SVPI_COMPLEN = 64;
  {$EXTERNALSYM SVPI_COPYRIGHTLEN}
  SVPI_COPYRIGHTLEN = 128;
  {$EXTERNALSYM SVI_MFGLEN}
  SVI_MFGLEN   = SVPI_MFGLEN;

  // Audio Errors 

  {$EXTERNALSYM AUDERR_NONE}
  AUDERR_NONE                   = S_OK;                         // 0x00000000
  {$EXTERNALSYM AUDERR_BADDEVICEID}
  AUDERR_BADDEVICEID            = HRESULT($80040301);
  {$EXTERNALSYM AUDERR_NEEDWAVEFORMAT}
  AUDERR_NEEDWAVEFORMAT         = HRESULT($80040302);
  {$EXTERNALSYM AUDERR_NOTSUPPORTED}
  AUDERR_NOTSUPPORTED           = E_NOTIMPL;                     // 0x80004001
  {$EXTERNALSYM AUDERR_NOTENOUGHDATA}
  AUDERR_NOTENOUGHDATA          = HRESULT($80040201);
  {$EXTERNALSYM AUDERR_NOTPLAYING}
  AUDERR_NOTPLAYING             = HRESULT($80040306);
  {$EXTERNALSYM AUDERR_INVALIDPARAM}
  AUDERR_INVALIDPARAM           = E_INVALIDARG;                  // 0x80070057
  {$EXTERNALSYM AUDERR_WAVEFORMATNOTSUPPORTED}
  AUDERR_WAVEFORMATNOTSUPPORTED = HRESULT($80040202);
  {$EXTERNALSYM AUDERR_WAVEDEVICEBUSY}
  AUDERR_WAVEDEVICEBUSY         = HRESULT($80040203);
  {$EXTERNALSYM AUDERR_WAVEDEVNOTSUPPORTED}
  AUDERR_WAVEDEVNOTSUPPORTED    = HRESULT($80040312);
  {$EXTERNALSYM AUDERR_NOTRECORDING}
  AUDERR_NOTRECORDING           = HRESULT($80040313);
  {$EXTERNALSYM AUDERR_INVALIDFLAG}
  AUDERR_INVALIDFLAG            = HRESULT($80040204);
  {$EXTERNALSYM AUDERR_INVALIDHANDLE}
  AUDERR_INVALIDHANDLE          = E_HANDLE;                      // 0x80070006
  {$EXTERNALSYM AUDERR_NODRIVER}
  AUDERR_NODRIVER               = HRESULT($80040317);
  {$EXTERNALSYM AUDERR_HANDLEBUSY}
  AUDERR_HANDLEBUSY             = HRESULT($80040318);
  {$EXTERNALSYM AUDERR_INVALIDNOTIFYSINK}
  AUDERR_INVALIDNOTIFYSINK      = HRESULT($80040319);
  {$EXTERNALSYM AUDERR_WAVENOTENABLED}
  AUDERR_WAVENOTENABLED         = HRESULT($8004031A);
  {$EXTERNALSYM AUDERR_ALREADYCLAIMED}
  AUDERR_ALREADYCLAIMED         = HRESULT($8004031D);
  {$EXTERNALSYM AUDERR_NOTCLAIMED}
  AUDERR_NOTCLAIMED             = HRESULT($8004031E);
  {$EXTERNALSYM AUDERR_STILLPLAYING}
  AUDERR_STILLPLAYING           = HRESULT($8004031F);
  {$EXTERNALSYM AUDERR_ALREADYSTARTED}
  AUDERR_ALREADYSTARTED         = HRESULT($80040320);
  {$EXTERNALSYM AUDERR_SYNCNOTALLOWED}
  AUDERR_SYNCNOTALLOWED         = HRESULT($80040321);

  //  Speech Recognition Warnings

  {$EXTERNALSYM SRWARN_BAD_LIST_PRONUNCIATION}
  SRWARN_BAD_LIST_PRONUNCIATION = HRESULT($80040401);

  //  Speech Recognition Errors

  {$EXTERNALSYM SRERR_NONE}
  SRERR_NONE                    = S_OK;                          // 0x00000000
  {$EXTERNALSYM SRERR_OUTOFDISK}
  SRERR_OUTOFDISK               = HRESULT($80040205);
  {$EXTERNALSYM SRERR_NOTSUPPORTED}
  SRERR_NOTSUPPORTED            = E_NOTIMPL;                     // 0x80004001
  {$EXTERNALSYM SRERR_NOTENOUGHDATA}
  SRERR_NOTENOUGHDATA           = AUDERR_NOTENOUGHDATA;          // 0x80040201
  {$EXTERNALSYM SRERR_VALUEOUTOFRANGE}
  SRERR_VALUEOUTOFRANGE         = E_UNEXPECTED;                  // 0x8000FFFF
  {$EXTERNALSYM SRERR_GRAMMARTOOCOMPLEX}
  SRERR_GRAMMARTOOCOMPLEX       = HRESULT($80040406);
  {$EXTERNALSYM SRERR_GRAMMARWRONGTYPE}
  SRERR_GRAMMARWRONGTYPE        = HRESULT($80040407);
  {$EXTERNALSYM SRERR_INVALIDWINDOW}
  SRERR_INVALIDWINDOW           = OLE_E_INVALIDHWND;             // 0x8004000F
  {$EXTERNALSYM SRERR_INVALIDPARAM}
  SRERR_INVALIDPARAM            = E_INVALIDARG;                  // 0x80070057
  {$EXTERNALSYM SRERR_INVALIDMODE}
  SRERR_INVALIDMODE             = HRESULT($80040206);
  {$EXTERNALSYM SRERR_TOOMANYGRAMMARS}
  SRERR_TOOMANYGRAMMARS         = HRESULT($8004040B);
  {$EXTERNALSYM SRERR_INVALIDLIST}
  SRERR_INVALIDLIST             = HRESULT($80040207);
  {$EXTERNALSYM SRERR_WAVEDEVICEBUSY}
  SRERR_WAVEDEVICEBUSY          = AUDERR_WAVEDEVICEBUSY;         // 0x80040203
  {$EXTERNALSYM SRERR_WAVEFORMATNOTSUPPORTED}
  SRERR_WAVEFORMATNOTSUPPORTED  = AUDERR_WAVEFORMATNOTSUPPORTED; // 0x80040202
  {$EXTERNALSYM SRERR_INVALIDCHAR}
  SRERR_INVALIDCHAR             = HRESULT($80040208);
  {$EXTERNALSYM SRERR_GRAMTOOCOMPLEX}
  SRERR_GRAMTOOCOMPLEX          = SRERR_GRAMMARTOOCOMPLEX;      // 0x80040406
  {$EXTERNALSYM SRERR_GRAMTOOLARGE}
  SRERR_GRAMTOOLARGE            = HRESULT($80040411);
  {$EXTERNALSYM SRERR_INVALIDINTERFACE}
  SRERR_INVALIDINTERFACE        = E_NOINTERFACE;                // 0x80004002
  {$EXTERNALSYM SRERR_INVALIDKEY}
  SRERR_INVALIDKEY              = HRESULT($80040209);
  {$EXTERNALSYM SRERR_INVALIDFLAG}
  SRERR_INVALIDFLAG             = AUDERR_INVALIDFLAG;           // 0x80040204
  {$EXTERNALSYM SRERR_GRAMMARERROR}
  SRERR_GRAMMARERROR            = HRESULT($80040416);
  {$EXTERNALSYM SRERR_INVALIDRULE}
  SRERR_INVALIDRULE             = HRESULT($80040417);
  {$EXTERNALSYM SRERR_RULEALREADYACTIVE}
  SRERR_RULEALREADYACTIVE       = HRESULT($80040418);
  {$EXTERNALSYM SRERR_RULENOTACTIVE}
  SRERR_RULENOTACTIVE           = HRESULT($80040419);
  {$EXTERNALSYM SRERR_NOUSERSELECTED}
  SRERR_NOUSERSELECTED          = HRESULT($8004041A);
  {$EXTERNALSYM SRERR_BAD_PRONUNCIATION}
  SRERR_BAD_PRONUNCIATION       = HRESULT($8004041B);
  {$EXTERNALSYM SRERR_DATAFILEERROR}
  SRERR_DATAFILEERROR           = HRESULT($8004041C);
  {$EXTERNALSYM SRERR_GRAMMARALREADYACTIVE}
  SRERR_GRAMMARALREADYACTIVE    = HRESULT($8004041D);           {SAPI 3}
  {$EXTERNALSYM SRERR_GRAMMARNOTACTIVE}
  SRERR_GRAMMARNOTACTIVE        = HRESULT($8004041E);           {SAPI 3}
  {$EXTERNALSYM SRERR_GLOBALGRAMMARALREADYACTIVE}
  SRERR_GLOBALGRAMMARALREADYACTIVE = HRESULT($8004041F);        {SAPI 3}
  {$EXTERNALSYM SRERR_LANGUAGEMISMATCH}
  SRERR_LANGUAGEMISMATCH        = HRESULT($80040420);           {SAPI 3}
  {$EXTERNALSYM SRERR_MULTIPLELANG}
  SRERR_MULTIPLELANG            = HRESULT($80040421);           {SAPI 3}
  {$EXTERNALSYM SRERR_LDGRAMMARNOWORDS}
  SRERR_LDGRAMMARNOWORDS        = HRESULT($80040422);           {SAPI 3}
  {$EXTERNALSYM SRERR_NOLEXICON}
  SRERR_NOLEXICON               = HRESULT($80040423);           {SAPI 3}
  {$EXTERNALSYM SRERR_SPEAKEREXISTS}
  SRERR_SPEAKEREXISTS           = HRESULT($80040424);           {SAPI 3}
  {$EXTERNALSYM SRERR_GRAMMARENGINEMISMATCH}
  SRERR_GRAMMARENGINEMISMATCH   = HRESULT($80040425);           {SAPI 3}
  {$EXTERNALSYM SRERR_BOOKMARKEXISTS}
  SRERR_BOOKMARKEXISTS          = HRESULT($80040426);           {SAPI 3}
  {$EXTERNALSYM SRERR_BOOKMARKDOESNOTEXIST}
  SRERR_BOOKMARKDOESNOTEXIST    = HRESULT($80040427);           {SAPI 3}
  {$EXTERNALSYM SRERR_MICWIZARDCANCELED}
  SRERR_MICWIZARDCANCELED       = HRESULT($80040428);           {SAPI 3}
  {$EXTERNALSYM SRERR_WORDTOOLONG}
  SRERR_WORDTOOLONG             = HRESULT($80040429);          {SAPI 4}
  {$EXTERNALSYM SRERR_BAD_WORD}
  SRERR_BAD_WORD                = HRESULT($8004042A);          {SAPI 4}
  {$EXTERNALSYM E_WRONGTYPE}
  E_WRONGTYPE                   = HRESULT($8004020C);          {SAPI 4}
  {$EXTERNALSYM E_BUFFERTOOSMALL}
  E_BUFFERTOOSMALL              = HRESULT($8004020D);          {SAPI 4}

  // Text to Speech Errors

  {$EXTERNALSYM TTSERR_NONE}
  TTSERR_NONE                   = S_OK;                         // 0x00000000
  {$EXTERNALSYM TTSERR_INVALIDINTERFACE}
  TTSERR_INVALIDINTERFACE       = E_NOINTERFACE;                // 0x80004002
  {$EXTERNALSYM TTSERR_OUTOFDISK}
  TTSERR_OUTOFDISK              = SRERR_OUTOFDISK;              // 0x80040205
  {$EXTERNALSYM TTSERR_NOTSUPPORTED}
  TTSERR_NOTSUPPORTED           = E_NOTIMPL;                    // 0x80004001
  {$EXTERNALSYM TTSERR_VALUEOUTOFRANGE}
  TTSERR_VALUEOUTOFRANGE        = E_UNEXPECTED;                 // 0x8000FFFF
  {$EXTERNALSYM TTSERR_INVALIDWINDOW}
  TTSERR_INVALIDWINDOW          = OLE_E_INVALIDHWND;            // 0x8004000F
  {$EXTERNALSYM TTSERR_INVALIDPARAM}
  TTSERR_INVALIDPARAM           = E_INVALIDARG;                 // 0x80070057
  {$EXTERNALSYM TTSERR_INVALIDMODE}
  TTSERR_INVALIDMODE            = SRERR_INVALIDMODE;            // 0x80040206
  {$EXTERNALSYM TTSERR_INVALIDKEY}
  TTSERR_INVALIDKEY             = SRERR_INVALIDKEY;             // 0x80040209
  {$EXTERNALSYM TTSERR_WAVEFORMATNOTSUPPORTED}
  TTSERR_WAVEFORMATNOTSUPPORTED = AUDERR_WAVEFORMATNOTSUPPORTED;// 0x80040202
  {$EXTERNALSYM TTSERR_INVALIDCHAR}
  TTSERR_INVALIDCHAR            = SRERR_INVALIDCHAR;            // 0x80040208
  {$EXTERNALSYM TTSERR_QUEUEFULL}
  TTSERR_QUEUEFULL              = HRESULT($8004020A);
  {$EXTERNALSYM TTSERR_WAVEDEVICEBUSY}
  TTSERR_WAVEDEVICEBUSY         = AUDERR_WAVEDEVICEBUSY;        // 0x80040203
  {$EXTERNALSYM TTSERR_NOTPAUSED}
  TTSERR_NOTPAUSED              = HRESULT($80040501);
  {$EXTERNALSYM TTSERR_ALREADYPAUSED}
  TTSERR_ALREADYPAUSED          = HRESULT($80040502);

  // Voice Command Errors

  {$EXTERNALSYM VCMDERR_NONE}
  VCMDERR_NONE                  = S_OK;                         // 0x00000000
  {$EXTERNALSYM VCMDERR_OUTOFMEM}
  VCMDERR_OUTOFMEM              = E_OUTOFMEMORY;                // 0x8007000E
  {$EXTERNALSYM VCMDERR_OUTOFDISK}
  VCMDERR_OUTOFDISK             = SRERR_OUTOFDISK;               // 0x80040205
  {$EXTERNALSYM VCMDERR_NOTSUPPORTED}
  VCMDERR_NOTSUPPORTED          = E_NOTIMPL;                     // 0x80004001
  {$EXTERNALSYM VCMDERR_VALUEOUTOFRANGE}
  VCMDERR_VALUEOUTOFRANGE       = E_UNEXPECTED;                  // 0x8000FFFF
  {$EXTERNALSYM VCMDERR_MENUTOOCOMPLEX}
  VCMDERR_MENUTOOCOMPLEX        = HRESULT($80040606);
  {$EXTERNALSYM VCMDERR_MENUWRONGLANGUAGE}
  VCMDERR_MENUWRONGLANGUAGE     = HRESULT($80040607);
  {$EXTERNALSYM VCMDERR_INVALIDWINDOW}
  VCMDERR_INVALIDWINDOW         = OLE_E_INVALIDHWND;             // 0x8004000F
  {$EXTERNALSYM VCMDERR_INVALIDPARAM}
  VCMDERR_INVALIDPARAM          = E_INVALIDARG;                  // 0x80070057
  {$EXTERNALSYM VCMDERR_INVALIDMODE}
  VCMDERR_INVALIDMODE           = SRERR_INVALIDMODE;             // 0x80040206
  {$EXTERNALSYM VCMDERR_TOOMANYMENUS}
  VCMDERR_TOOMANYMENUS          = HRESULT($8004060B);
  {$EXTERNALSYM VCMDERR_INVALIDLIST}
  VCMDERR_INVALIDLIST           = SRERR_INVALIDLIST;
  {$EXTERNALSYM VCMDERR_MENUDOESNOTEXIST}
  VCMDERR_MENUDOESNOTEXIST      = HRESULT($8004060D);
  {$EXTERNALSYM VCMDERR_MENUACTIVE}
  VCMDERR_MENUACTIVE            = HRESULT($8004060E);
  {$EXTERNALSYM VCMDERR_NOENGINE}
  VCMDERR_NOENGINE              = HRESULT($8004060F);
  {$EXTERNALSYM VCMDERR_NOGRAMMARINTERFACE}
  VCMDERR_NOGRAMMARINTERFACE    = HRESULT($80040610);
  {$EXTERNALSYM VCMDERR_NOFINDINTERFACE}
  VCMDERR_NOFINDINTERFACE       = HRESULT($80040611);
  {$EXTERNALSYM VCMDERR_CANTCREATESRENUM}
  VCMDERR_CANTCREATESRENUM      = HRESULT($80040612);
  {$EXTERNALSYM VCMDERR_NOSITEINFO}
  VCMDERR_NOSITEINFO            = HRESULT($80040613);
  {$EXTERNALSYM VCMDERR_SRFINDFAILED}
  VCMDERR_SRFINDFAILED          = HRESULT($80040614);
  {$EXTERNALSYM VCMDERR_CANTCREATEAUDIODEVICE}
  VCMDERR_CANTCREATEAUDIODEVICE = HRESULT($80040615);
  {$EXTERNALSYM VCMDERR_CANTSETDEVICE}
  VCMDERR_CANTSETDEVICE         = HRESULT($80040616);
  {$EXTERNALSYM VCMDERR_CANTSELECTENGINE}
  VCMDERR_CANTSELECTENGINE      = HRESULT($80040617);
  {$EXTERNALSYM VCMDERR_CANTCREATENOTIFY}
  VCMDERR_CANTCREATENOTIFY      = HRESULT($80040618);
  {$EXTERNALSYM VCMDERR_CANTCREATEDATASTRUCTURES}
  VCMDERR_CANTCREATEDATASTRUCTURES = HRESULT($80040619);
  {$EXTERNALSYM VCMDERR_CANTINITDATASTRUCTURES}
  VCMDERR_CANTINITDATASTRUCTURES   = HRESULT($8004061A);
  {$EXTERNALSYM VCMDERR_NOCACHEDATA}
  VCMDERR_NOCACHEDATA           = HRESULT($8004061B);
  {$EXTERNALSYM VCMDERR_NOCOMMANDS}
  VCMDERR_NOCOMMANDS            = HRESULT($8004061C);
  {$EXTERNALSYM VCMDERR_CANTXTRACTWORDS}
  VCMDERR_CANTXTRACTWORDS       = HRESULT($8004061D);
  {$EXTERNALSYM VCMDERR_CANTGETDBNAME}
  VCMDERR_CANTGETDBNAME         = HRESULT($8004061E);
  {$EXTERNALSYM VCMDERR_CANTCREATEKEY}
  VCMDERR_CANTCREATEKEY         = HRESULT($8004061F);
  {$EXTERNALSYM VCMDERR_CANTCREATEDBNAME}
  VCMDERR_CANTCREATEDBNAME      = HRESULT($80040620);
  {$EXTERNALSYM VCMDERR_CANTUPDATEREGISTRY}
  VCMDERR_CANTUPDATEREGISTRY    = HRESULT($80040621);
  {$EXTERNALSYM VCMDERR_CANTOPENREGISTRY}
  VCMDERR_CANTOPENREGISTRY      = HRESULT($80040622);
  {$EXTERNALSYM VCMDERR_CANTOPENDATABASE}
  VCMDERR_CANTOPENDATABASE      = HRESULT($80040623);
  {$EXTERNALSYM VCMDERR_CANTCREATESTORAGE}
  VCMDERR_CANTCREATESTORAGE     = HRESULT($80040624);
  {$EXTERNALSYM VCMDERR_CANNOTMIMIC}
  VCMDERR_CANNOTMIMIC           = HRESULT($80040625);
  {$EXTERNALSYM VCMDERR_MENUEXIST}
  VCMDERR_MENUEXIST             = HRESULT($80040626);
  {$EXTERNALSYM VCMDERR_MENUOPEN}
  VCMDERR_MENUOPEN              = HRESULT($80040627);

  // Voice Text Errors

  {$EXTERNALSYM VTXTERR_NONE}
  VTXTERR_NONE                  = S_OK;                          // 0x00000000
  {$EXTERNALSYM VTXTERR_OUTOFMEM}
  VTXTERR_OUTOFMEM              = E_OUTOFMEMORY;                 // 0x8007000E
  {$EXTERNALSYM VTXTERR_EMPTYSPEAKSTRING}
  VTXTERR_EMPTYSPEAKSTRING      = HRESULT($8004020B);
  {$EXTERNALSYM VTXTERR_INVALIDPARAM}
  VTXTERR_INVALIDPARAM          = E_INVALIDARG;                  // 0x80070057
  {$EXTERNALSYM VTXTERR_INVALIDMODE}
  VTXTERR_INVALIDMODE           = SRERR_INVALIDMODE;             // 0x80040206
  {$EXTERNALSYM VTXTERR_NOENGINE}
  VTXTERR_NOENGINE              = HRESULT($8004070F);
  {$EXTERNALSYM VTXTERR_NOFINDINTERFACE}
  VTXTERR_NOFINDINTERFACE       = HRESULT($80040711);
  {$EXTERNALSYM VTXTERR_CANTCREATETTSENUM}
  VTXTERR_CANTCREATETTSENUM     = HRESULT($80040712);
  {$EXTERNALSYM VTXTERR_NOSITEINFO}
  VTXTERR_NOSITEINFO            = HRESULT($80040713);
  {$EXTERNALSYM VTXTERR_TTSFINDFAILED}
  VTXTERR_TTSFINDFAILED         = HRESULT($80040714);
  {$EXTERNALSYM VTXTERR_CANTCREATEAUDIODEVICE}
  VTXTERR_CANTCREATEAUDIODEVICE = HRESULT($80040715);
  {$EXTERNALSYM VTXTERR_CANTSETDEVICE}
  VTXTERR_CANTSETDEVICE         = HRESULT($80040716);
  {$EXTERNALSYM VTXTERR_CANTSELECTENGINE}
  VTXTERR_CANTSELECTENGINE      = HRESULT($80040717);
  {$EXTERNALSYM VTXTERR_CANTCREATENOTIFY}
  VTXTERR_CANTCREATENOTIFY      = HRESULT($80040718);
  {$EXTERNALSYM VTXTERR_NOTENABLED}
  VTXTERR_NOTENABLED            = HRESULT($80040719);
  {$EXTERNALSYM VTXTERR_OUTOFDISK}
  VTXTERR_OUTOFDISK             = SRERR_OUTOFDISK;               // 0x80040205
  {$EXTERNALSYM VTXTERR_NOTSUPPORTED}
  VTXTERR_NOTSUPPORTED          = E_NOTIMPL;                     // 0x80004001
  {$EXTERNALSYM VTXTERR_NOTENOUGHDATA}
  VTXTERR_NOTENOUGHDATA         = AUDERR_NOTENOUGHDATA;          // 0x80040201
  {$EXTERNALSYM VTXTERR_QUEUEFULL}
  VTXTERR_QUEUEFULL             = TTSERR_QUEUEFULL;              // 0x8004020A
  {$EXTERNALSYM VTXTERR_VALUEOUTOFRANGE}
  VTXTERR_VALUEOUTOFRANGE       = E_UNEXPECTED;                  // 0x8000FFFF
  {$EXTERNALSYM VTXTERR_INVALIDWINDOW}
  VTXTERR_INVALIDWINDOW         = OLE_E_INVALIDHWND;             // 0x8004000F
  {$EXTERNALSYM VTXTERR_WAVEDEVICEBUSY}
  VTXTERR_WAVEDEVICEBUSY        = AUDERR_WAVEDEVICEBUSY;         // 0x80040203
  {$EXTERNALSYM VTXTERR_WAVEFORMATNOTSUPPORTED}
  VTXTERR_WAVEFORMATNOTSUPPORTED = AUDERR_WAVEFORMATNOTSUPPORTED; // 0x80040202
  {$EXTERNALSYM VTXTERR_INVALIDCHAR}
  VTXTERR_INVALIDCHAR           = SRERR_INVALIDCHAR;             // 0x80040208

  // ILexPronounce errors
  {$EXTERNALSYM LEXERR_INVALIDTEXTCHAR}
  LEXERR_INVALIDTEXTCHAR        = HRESULT($80040801);
  {$EXTERNALSYM LEXERR_INVALIDSENSE}
  LEXERR_INVALIDSENSE           = HRESULT($80040802);
  {$EXTERNALSYM LEXERR_NOTINLEX}
  LEXERR_NOTINLEX               = HRESULT($80040803);
  {$EXTERNALSYM LEXERR_OUTOFDISK}
  LEXERR_OUTOFDISK              = HRESULT($80040804);
  {$EXTERNALSYM LEXERR_INVALIDPRONCHAR}
  LEXERR_INVALIDPRONCHAR        = HRESULT($80040805);
  {$EXTERNALSYM LEXERR_ALREADYINLEX}
  LEXERR_ALREADYINLEX           = HRESULT($00040806);
  {$EXTERNALSYM LEXERR_PRNBUFTOOSMALL}
  LEXERR_PRNBUFTOOSMALL         = HRESULT($80040807);
  {$EXTERNALSYM LEXERR_ENGBUFTOOSMALL}
  LEXERR_ENGBUFTOOSMALL         = HRESULT($80040808);
  {$EXTERNALSYM LEXERR_INVALIDLEX}
  LEXERR_INVALIDLEX             = HRESULT($80040809);

type

  PSData = ^TSData;
  {$EXTERNALSYM SDATA}
  SDATA = record
    pData: pointer;
    dwSize: DWORD;
  end;
  TSData = SDATA;


  PLanguageA = ^TLanguageA;
  PLanguageW = ^TLanguageW;
  PLanguage = PLanguageA;
  {$EXTERNALSYM LANGUAGEA}
  LANGUAGEA = record
    LanguageID: LangID;
    szDialect: array[0..LANG_LEN - 1] of AnsiChar;
  end;
  {$EXTERNALSYM LANGUAGEW}
  LANGUAGEW = record
    LanguageID: LangID;
    szDialect: array[0..LANG_LEN - 1] of WideChar;
  end;
  {$EXTERNALSYM LANGUAGE}
  LANGUAGE = LANGUAGEA;
  TLanguageA = LANGUAGEA;
  TLanguageW = LANGUAGEW;
  TLanguage = TLanguageA;

  PSpchErrorA = ^TSpchErrorA;
  PSpchErrorW = ^TSpchErrorW;
  PSpchError = PSpchErrorA;
  {$EXTERNALSYM SPCHERRORA}
  SPCHERRORA = record
    hRes: HRESULT;
    szStrings: array[0..511] of AnsiChar;
  end;
  {$EXTERNALSYM SPCHERRORW}
  SPCHERRORW = record
    hRes: HRESULT;
    szStrings: array[0..511] of WideChar;
  end;
  {$EXTERNALSYM SPCHERROR}
  SPCHERROR = SPCHERRORA;
  TSpchErrorA = SPCHERRORA;
  TSpchErrorW = SPCHERRORW;
  TSpchError = TSpchErrorA;

  {$EXTERNALSYM QWORD}
  QWORD  = Int64;
  {$EXTERNALSYM PQWORD}
  PQWORD = ^QWORD;


const
  {$EXTERNALSYM CHARSET_TEXT}
  CHARSET_TEXT = 0;
  {$EXTERNALSYM CHARSET_IPAPHONETIC}
  CHARSET_IPAPHONETIC = 1;
  {$EXTERNALSYM CHARSET_ENGINEPHONETIC}
  CHARSET_ENGINEPHONETIC = 2;

type
  {$EXTERNALSYM VOICECHARSET}
  VOICECHARSET = UINT;

const
  {$EXTERNALSYM VPS_UNKNOWN}
  VPS_UNKNOWN = 0;
  {$EXTERNALSYM VPS_NOUN}
  VPS_NOUN = 1;
  {$EXTERNALSYM VPS_VERB}
  VPS_VERB = 2;
  {$EXTERNALSYM VPS_ADVERB}
  VPS_ADVERB = 3;
  {$EXTERNALSYM VPS_ADJECTIVE}
  VPS_ADJECTIVE = 4;
  {$EXTERNALSYM VPS_PROPERNOUN}
  VPS_PROPERNOUN = 5;
  {$EXTERNALSYM VPS_PRONOUN}
  VPS_PRONOUN = 6;
  {$EXTERNALSYM VPS_CONJUNCTION}
  VPS_CONJUNCTION = 7;
  {$EXTERNALSYM VPS_CARDINAL}
  VPS_CARDINAL = 8;
  {$EXTERNALSYM VPS_ORDINAL}
  VPS_ORDINAL = 9;
  {$EXTERNALSYM VPS_DETERMINER}
  VPS_DETERMINER = 10;
  {$EXTERNALSYM VPS_QUANTIFIER}
  VPS_QUANTIFIER = 11;
  {$EXTERNALSYM VPS_PUNCTUATION}
  VPS_PUNCTUATION = 12;
  {$EXTERNALSYM VPS_CONTRACTION}
  VPS_CONTRACTION = 13;
  {$EXTERNALSYM VPS_INTERJECTION}
  VPS_INTERJECTION = 14;
  {$EXTERNALSYM VPS_ABBREVIATION}
  VPS_ABBREVIATION = 15;
  {$EXTERNALSYM VPS_PREPOSITION}
  VPS_PREPOSITION = 16;

type
  {$EXTERNALSYM VOICEPARTOFSPEECH}
  VOICEPARTOFSPEECH = UINT;
  TVoicePartOfSpeech = VOICEPARTOFSPEECH;

  PSRResPhonemeNode = ^TSRResPhonemeNode;
  {$EXTERNALSYM SRRESPHONEMENODE}
  SRRESPHONEMENODE = record
    dwNextPhonemeNode          : DWORD;
    dwUpAlternatePhonemeNode   : DWORD;
    dwDownAlternatePhonemeNode : DWORD;
    dwPreviousPhonemeNode      : DWORD;
    dwWordNode                 : DWORD;
    qwStartTime                : QWORD;
    qwEndTime                  : QWORD;
    dwPhonemeScore             : DWORD;
    wVolume                    : WORD;
    wPitch                     : WORD;
  end;
  TSRResPhonemeNode = SRRESPHONEMENODE;

  PSRResWordNode = ^TSRResWordNode;
  {$EXTERNALSYM SRRESWORDNODE}
  SRRESWORDNODE = record
    dwNextWordNode          : DWORD;
    dwUpAlternateWordNode   : DWORD;
    dwDownAlternateWordNode : DWORD;
    dwPreviousWordNode      : DWORD;
    dwPhonemeNode           : DWORD;
    qwStartTime             : QWORD;
    qwEndTime               : QWORD;
    dwWordScore             : DWORD;
    wVolume                 : WORD;
    wPitch                  : WORD;
    pos                     : VOICEPARTOFSPEECH;
    dwCFGParse              : DWORD;
    dwCue                   : DWORD;
  end;
  TSRResWordNode = SRRESWORDNODE;

const
  // IAttribute Constants                            //SAPI 4.0
  {$EXTERNALSYM TTSBASEATTR}
  TTSBASEATTR             = $1000;                     //SAPI 4.0
  {$EXTERNALSYM SRBASEATTR}
  SRBASEATTR              = $2000;                     //SAPI 4.0
  {$EXTERNALSYM VDCTBASEATTR}
  VDCTBASEATTR            = $3000;                     //SAPI 4.0
  {$EXTERNALSYM VCMDBASEATTR}
  VCMDBASEATTR            = $4000;                     //SAPI 4.0
  {$EXTERNALSYM VTXTBASEATTR}
  VTXTBASEATTR            = $5000;                     //SAPI 4.0
  {$EXTERNALSYM AUDBASEATTR}
  AUDBASEATTR             = $6000;

  {$EXTERNALSYM TTSATTR_PITCH}
  TTSATTR_PITCH           = 1;                         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_REALTIME}
  TTSATTR_REALTIME        = 0;                         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEED}
  TTSATTR_SPEED           = 2;                         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_VOLUME}
  TTSATTR_VOLUME          = 3;                         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGE}
  TTSATTR_PITCHRANGE      = TTSBASEATTR + 5;           //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGEDEFAULT}
  TTSATTR_PITCHRANGEDEFAULT = TTSBASEATTR + 6;         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGEMAX}
  TTSATTR_PITCHRANGEMAX   = TTSBASEATTR + 7;           //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGEMIN}
  TTSATTR_PITCHRANGEMIN   = TTSBASEATTR + 8;           //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGERELATIVE}
  TTSATTR_PITCHRANGERELATIVE = TTSBASEATTR + 9;        //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGERELATIVEMAX}
  TTSATTR_PITCHRANGERELATIVEMAX = TTSBASEATTR + 10;    //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRANGERELATIVEMIN}
  TTSATTR_PITCHRANGERELATIVEMIN = TTSBASEATTR + 11;    //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRELATIVE}
  TTSATTR_PITCHRELATIVE   = TTSBASEATTR + 12;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRELATIVEMAX}
  TTSATTR_PITCHRELATIVEMAX = TTSBASEATTR + 13;         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHRELATIVEMIN}
  TTSATTR_PITCHRELATIVEMIN = TTSBASEATTR + 14;         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHDEFAULT}
  TTSATTR_PITCHDEFAULT    = TTSBASEATTR + 15;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHMAX}
  TTSATTR_PITCHMAX        = TTSBASEATTR + 16;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_PITCHMIN}
  TTSATTR_PITCHMIN        = TTSBASEATTR + 17;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEEDRELATIVE}
  TTSATTR_SPEEDRELATIVE   = TTSBASEATTR + 18;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEEDRELATIVEMAX}
  TTSATTR_SPEEDRELATIVEMAX = TTSBASEATTR + 19;         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEEDRELATIVEMIN}
  TTSATTR_SPEEDRELATIVEMIN = TTSBASEATTR + 20;         //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEEDDEFAULT}
  TTSATTR_SPEEDDEFAULT    = TTSBASEATTR + 21;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEEDMAX}
  TTSATTR_SPEEDMAX        = TTSBASEATTR + 22;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SPEEDMIN}
  TTSATTR_SPEEDMIN        = TTSBASEATTR + 23;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_THREADPRIORITY}
  TTSATTR_THREADPRIORITY  = TTSBASEATTR + 24;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_SINKFLAGS}
  TTSATTR_SINKFLAGS       = TTSBASEATTR + 25;          //SAPI 4.0
  {$EXTERNALSYM TTSATTR_VOLUMEDEFAULT}
  TTSATTR_VOLUMEDEFAULT   = TTSBASEATTR + 26;          //SAPI 4.0

  {$EXTERNALSYM SRATTR_AUTOGAIN}
  SRATTR_AUTOGAIN         = 1;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_ECHO}
  SRATTR_ECHO             = 3;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_ENERGYFLOOR}
  SRATTR_ENERGYFLOOR      = 4;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_MICROPHONE}
  SRATTR_MICROPHONE       = 5;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_REALTIME}
  SRATTR_REALTIME         = 6;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_SPEAKER}
  SRATTR_SPEAKER          = 7;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_TIMEOUT_COMPLETE}
  SRATTR_TIMEOUT_COMPLETE = 8;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_TIMEOUT_INCOMPLETE}
  SRATTR_TIMEOUT_INCOMPLETE = SRBASEATTR + 8;          //SAPI 4.0
  {$EXTERNALSYM SRATTR_THRESHOLD}
  SRATTR_THRESHOLD        = 2;                         //SAPI 4.0
  {$EXTERNALSYM SRATTR_ACCURACYSLIDER}
  SRATTR_ACCURACYSLIDER   = SRBASEATTR + 10;           //SAPI 4.0
  {$EXTERNALSYM SRATTR_LEVEL}
  SRATTR_LEVEL            = SRBASEATTR + 11;           //SAPI 4.0
  {$EXTERNALSYM SRATTR_LISTENINGSTATE}
  SRATTR_LISTENINGSTATE   = SRBASEATTR + 12;           //SAPI 4.0
  {$EXTERNALSYM SRATTR_RESULTSINFO}
  SRATTR_RESULTSINFO      = SRBASEATTR + 13;           //SAPI 4.0
  {$EXTERNALSYM SRATTR_RESULTSINFO_POSSIBLE}
  SRATTR_RESULTSINFO_POSSIBLE = SRBASEATTR + 14;       //SAPI 4.0
  {$EXTERNALSYM SRATTR_SINKFLAGS}
  SRATTR_SINKFLAGS        = SRBASEATTR + 15;           //SAPI 4.0
  {$EXTERNALSYM SRATTR_THREADPRIORITY}
  SRATTR_THREADPRIORITY   = SRBASEATTR + 16;           //SAPI 4.0

  {$EXTERNALSYM VDCTATTR_AWAKESTATE}
  VDCTATTR_AWAKESTATE     = VDCTBASEATTR + 1;          //SAPI 4.0
  {$EXTERNALSYM VDCTATTR_MODE}
  VDCTATTR_MODE           = VDCTBASEATTR + 2;          //SAPI 4.0
  {$EXTERNALSYM VDCTATTR_MEMORY}
  VDCTATTR_MEMORY         = VDCTBASEATTR + 3;          //SAPI 4.0
  {$EXTERNALSYM VDCTATTR_CORRECTIONRECT}
  VDCTATTR_CORRECTIONRECT = VDCTBASEATTR + 4;          //SAPI 4.0
  {$EXTERNALSYM VDCTATTR_VISIBLETEXTSTART}
  VDCTATTR_VISIBLETEXTSTART = VDCTBASEATTR + 5;        //SAPI 4.0
  {$EXTERNALSYM VDCTATTR_VISIBLETEXTCHARS}
  VDCTATTR_VISIBLETEXTCHARS = VDCTBASEATTR + 6;        //SAPI 4.0
  {$EXTERNALSYM VDCTATTR_INSERTIONGUI}
  VDCTATTR_INSERTIONGUI   = VDCTBASEATTR + 7;   // turn on ISRGramInsertionGUI //SAPI 4.0

  {$EXTERNALSYM VCMDATTR_AWAKESTATE}
  VCMDATTR_AWAKESTATE     = VCMDBASEATTR + 1;          //SAPI 4.0
  {$EXTERNALSYM VCMDATTR_DEVICE}
  VCMDATTR_DEVICE         = VCMDBASEATTR + 2;          //SAPI 4.0
  {$EXTERNALSYM VCMDATTR_ENABLED}
  VCMDATTR_ENABLED        = VCMDBASEATTR + 3;          //SAPI 4.0
  {$EXTERNALSYM VCMDATTR_SRMODE}
  VCMDATTR_SRMODE         = VCMDBASEATTR + 4;          //SAPI 4.0

  {$EXTERNALSYM AUDATTR_USELOWPRIORITY}
  AUDATTR_USELOWPRIORITY  = AUDBASEATTR + 1; // MMAudioSource: if true, send low priority message to card when starting
  {$EXTERNALSYM AUDATTR_AUTORETRY}
  AUDATTR_AUTORETRY       = AUDBASEATTR + 2; // MMAudioSource: if true, keep retrying to open wave device if can't initially
  {$EXTERNALSYM AUDATTR_TIMERMS}
  AUDATTR_TIMERMS         = AUDBASEATTR + 3; // MMAudioDest: milliseconds between timer ticks, when send out buffers, etc.
  {$EXTERNALSYM AUDATTR_DIRECTFLAGS}
  AUDATTR_DIRECTFLAGS     = AUDBASEATTR + 4; // DirectSoundAudioDest: Flages to send into CreateSoundBuffer
                                             // set these values before waveformatset. Use to do 3-d tts

  // SRATTR_RESULTSINFO flags                          //SAPI 4.0
  {$EXTERNALSYM SRRI_AUDIO		}
  SRRI_AUDIO		  = $00000001 ;                //SAPI 4.0
  {$EXTERNALSYM SRRI_AUDIO_UNCOMPRESSED}
  SRRI_AUDIO_UNCOMPRESSED = $00000002 ;                //SAPI 4.0
  {$EXTERNALSYM SRRI_ALTERNATIVES}
  SRRI_ALTERNATIVES       = $00000004 ;                //SAPI 4.0
  {$EXTERNALSYM SRRI_WORDGRAPH}
  SRRI_WORDGRAPH          = $00000008 ;                //SAPI 4.0
  {$EXTERNALSYM SRRI_PHONEMEGRAPH}
  SRRI_PHONEMEGRAPH       = $00000010 ;                //SAPI 4.0

  // SRATTR_SINKFLAGS flags                            //SAPI 4.0
  {$EXTERNALSYM SRASF_ATTRIBUTES}
  SRASF_ATTRIBUTES        = $00000001 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_INTERFERENCE}
  SRASF_INTERFERENCE      = $00000002 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_SOUND}
  SRASF_SOUND             = $00000004 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_UTTERANCEBEGIN}
  SRASF_UTTERANCEBEGIN    = $00000008 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_UTTERANCEEND}
  SRASF_UTTERANCEEND      = $00000010 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_VUMETER}
  SRASF_VUMETER           = $00000020 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_PHRASEHYPOTHESIS}
  SRASF_PHRASEHYPOTHESIS  = $00000040 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_TRAINING}
  SRASF_TRAINING          = $00000080 ;                //SAPI 4.0
  {$EXTERNALSYM SRASF_ERRORWARNING}
  SRASF_ERRORWARNING      = $00000100 ;                //SAPI 4.0


// IAttributes                                          //SAPI 4.0
type
  {$EXTERNALSYM IAttributesA}
  IAttributesA = interface(IUnknown)
    [SID_IAttributesA]
    function DWordGet(dwAttrib: DWORD; var dwValue: DWORD): HResult; stdcall;
    function DWordSet(dwAttrib: DWORD; dwValue: DWORD): HResult; stdcall;
    function StringGet (dwAttrib: DWORD; sz: PAnsiChar; dwSize: DWORD;
       var dwNeeded: DWORD): HResult; stdcall;
    function StringSet (dwAttrib: DWORD; sz: PAnsiChar): HResult; stdcall;
    function MemoryGet(dwAttrib: DWORD; var pMem: pointer; var dwSize: DWORD): HResult; stdcall;
    function MemorySet (dwAttrib: DWORD; pMem: Pointer; dwSize: DWORD): HResult; stdcall;
  end; //IAttributes
  {$EXTERNALSYM PIATTRIBUTESA}
  PIATTRIBUTESA = ^IAttributesA;
  {$EXTERNALSYM IAttributesW}
  IAttributesW = interface(IUnknown)
    [SID_IAttributesW]
    function DWordGet(dwAttrib: DWORD; var dwValue: DWORD): HResult; stdcall;
    function DWordSet(dwAttrib: DWORD; dwValue: DWORD): HResult; stdcall;
    function StringGet (dwAttrib: DWORD; sz: PWideChar; dwSize: DWORD;
       var dwNeeded: DWORD): HResult; stdcall;
    function StringSet (dwAttrib: DWORD; sz: PWideChar): HResult; stdcall;
    function MemoryGet(dwAttrib: DWORD; var pMem: pointer; var dwSize: DWORD): HResult; stdcall;
    function MemorySet (dwAttrib: DWORD; pMem: Pointer; dwSize: DWORD): HResult; stdcall;
  end; //IAttributes
  {$EXTERNALSYM PIATTRIBUTESW}
  PIATTRIBUTESW = ^IAttributesW;
  {$EXTERNALSYM IAttributes}
  IAttributes = IAttributesA;

// ISpchError                                               //SAPI 4

  {$EXTERNALSYM ISpchErrorA}
  ISpchErrorA = interface(IUnknown)
    [SID_ISpchErrorA]
    function LastErrorGet(pError: SPCHERRORA): HResult; stdcall;
    function ErrorMessageGet(pszMessage: PAnsiChar; dwMessageSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
   end;
  {$EXTERNALSYM PISPCHERRORA}
   PISPCHERRORA = ^ISpchErrorA;
  {$EXTERNALSYM ISpchErrorW}
  ISpchErrorW = interface(IUnknown)
    [SID_ISpchErrorW]
    function LastErrorGet(pError: SPCHERRORW): HResult; stdcall;
    function ErrorMessageGet(pszMessage: PWideChar; dwMessageSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
   end;
  {$EXTERNALSYM PISPCHERRORW}
   PISPCHERRORW = ^ISpchErrorW;
  {$EXTERNALSYM ISpchError}
  ISpchError = ISpchErrorA;

  {$EXTERNALSYM ILexPronounceA}
  ILexPronounceA = interface(IUnknown)
    [SID_ILexPronounceA]
    function Add(CharSet: VOICECHARSET; pszText: PAnsiChar; pszPronounce: PAnsiChar;
      PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: Pointer;
      dwEngineInfoSize: DWORD): HResult; stdcall;
    function Get(CharSet: VOICECHARSET; pszText: PAnsiChar; wSense: WORD;
      pszPronounce: PAnsiChar; dwPronounceSize: DWORD; var dwPronounceNeeded: DWORD;
      var PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: pointer;
      dwEngineInfoSize: DWORD; pdwEngineInfoNeeded: PDWORD): HResult; stdcall;
    function Remove(pszText: PAnsiChar; wSense: WORD): HResult; stdcall;
  end; //ILexPronounce
  {$EXTERNALSYM PILEXPRONOUNCEA}
  PILEXPRONOUNCEA = ^ILexPronounceA;
  {$EXTERNALSYM ILexPronounceW}
  ILexPronounceW = interface(IUnknown)
    [SID_ILexPronounceW]
    function Add(CharSet: VOICECHARSET; pszText: PWideChar; pszPronounce: PWideChar;
      PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: Pointer;
      dwEngineInfoSize: DWORD): HResult; stdcall;
    function Get(CharSet: VOICECHARSET; pszText: PWideChar; wSense: WORD;
      pszPronounce: PWideChar; dwPronounceSize: DWORD; var dwPronounceNeeded: DWORD;
      var PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: pointer;
      dwEngineInfoSize: DWORD; pdwEngineInfoNeeded: PDWORD): HResult; stdcall;
    function Remove(pszText: PWideChar; wSense: WORD): HResult; stdcall;
  end; //ILexPronounce
  {$EXTERNALSYM PILEXPRONOUNCEW}
  PILEXPRONOUNCEW = ^ILexPronounceW;
  {$EXTERNALSYM ILexPronounce}
  ILexPronounce = ILexPronounceA;

const
  {$EXTERNALSYM ILP2_ACTIVE}
  ILP2_ACTIVE             = $00000001 ;                //SAPI 4.0
  {$EXTERNALSYM ILP2_USER}
  ILP2_USER               = $00000002 ;                //SAPI 4.0
  {$EXTERNALSYM ILP2_BACKUP}
  ILP2_BACKUP             = $00000004 ;                //SAPI 4.0
  {$EXTERNALSYM ILP2_LTS}
  ILP2_LTS                = $00000008 ;                //SAPI 4.0

type
  {$EXTERNALSYM ILexPronounce2A}
  ILexPronounce2A = interface(ILexPronounceA)
    [SID_ILexPronounce2A]
    function AddTo(dwLexicon: DWORD; CharSet: VOICECHARSET; pszText: PAnsiChar;
      pszPronounce: PAnsiChar; PartOfSpeech: VOICEPARTOFSPEECH;
      pEngineInfo: pointer; dwEngineInfoSize: DWORD): HResult; stdcall;
    function GetFrom(dwLexicon: DWORD; CharSet: VOICECHARSET; pszText: PAnsiChar;
      wSense: WORD; pszPronounce: PAnsiChar; dwPronounceSize: DWORD;
      var dwPronounceNeeded: DWORD; var PartOfSpeech: VOICEPARTOFSPEECH;
      pEngineInfo: pointer; dwEngineInfoSize: DWORD;
      pdwEngineInfoNeeded: PDWORD): HResult; stdcall;
    function RemoveFrom(dwLexicon: DWORD; pszText: PAnsiChar;
      wSense: WORD): HResult; stdcall;
    function QueryLexicons(fWrite: BOOL; var dwLexicon: DWORD): HResult; stdcall;
    function ChangeSpelling(dwLexicon: DWORD; pszOrg: PAnsiChar;
      pszNew: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PILEXPRONOUNCE2A}
  PILEXPRONOUNCE2A = ^ILexPronounce2A;
  {$EXTERNALSYM ILexPronounce2W}
  ILexPronounce2W = interface(ILexPronounceW)
    [SID_ILexPronounce2W]
    function AddTo(dwLexicon: DWORD; CharSet: VOICECHARSET; pszText: PWideChar;
      pszPronounce: PWideChar; PartOfSpeech: VOICEPARTOFSPEECH;
      pEngineInfo: pointer; dwEngineInfoSize: DWORD): HResult; stdcall;
    function GetFrom(dwLexicon: DWORD; CharSet: VOICECHARSET; pszText: PWideChar;
      wSense: WORD; pszPronounce: PWideChar; dwPronounceSize: DWORD;
      var dwPronounceNeeded: DWORD; var PartOfSpeech: VOICEPARTOFSPEECH;
      pEngineInfo: pointer; dwEngineInfoSize: DWORD;
      pdwEngineInfoNeeded: PDWORD): HResult; stdcall;
    function RemoveFrom(dwLexicon: DWORD; pszText: PWideChar;
      wSense: WORD): HResult; stdcall;
    function QueryLexicons(fWrite: BOOL; var dwLexicon: DWORD): HResult; stdcall;
    function ChangeSpelling(dwLexicon: DWORD; pszOrg: PWideChar;
      pszNew: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PILEXPRONOUNCE2W}
  PILEXPRONOUNCE2W = ^ILexPronounce2W;
  {$EXTERNALSYM ILexPronounce2}
  ILexPronounce2 = ILexPronounce2A;

// Speech Tools
// ISTMicWizard Wizard
const
  Default_Hz =   16000;      // Not part of published API but part of Doc - Possible Values for SamplesPerSec

  { Possible Values for Use }
  {$EXTERNALSYM STMWU_CNC}
  STMWU_CNC            = 0; //{The wizard is setting the system up for command and control. It will be less strict about audio record quality.}
  {$EXTERNALSYM STMWU_DICTATION}
  STMWU_DICTATION      = 1; //{The wizard is setting the system up for dictation and will be strict about sound quality. }
  {$EXTERNALSYM STMWU_LOWERGAIN}
  STMWU_LOWERGAIN      = $10000 ;  //{if on, tries for a lower gain}
  {$EXTERNALSYM STMWU_NOAUTOGAIN}
  STMWU_NOAUTOGAIN     = $20000 ;  //{if on, doesn't adjust the autogain bit}

  {Possible Values for Flags}
  {$EXTERNALSYM STMWF_CANSKIP}
  STMWF_CANSKIP        = 1; //{If the microphone setup wizard has already been run by the user then just return success. An application can}
                            //{run ISTMicWizard->Wizard() every time it starts, using STMWF_CANSKIP, so that the user is guaranteed to have microphone.}

  {Possible Values for dwMicrophoneType - MICWIZARDINFO }
  {$EXTERNALSYM STMWI_UNKNOWN}
  STMWI_UNKNOWN    = 0; // Unknown
  {$EXTERNALSYM STMWI_CLOSETALK}
  STMWI_CLOSETALK  = 1; // Close-talk or headset microphone
  {$EXTERNALSYM STMWI_EARPIECE}
  STMWI_EARPIECE   = 2; // Microphone with an element near the ear or cheek bone
  {$EXTERNALSYM STMWI_HANDSET}
  STMWI_HANDSET    = 3; // Telephone-style handset
  {$EXTERNALSYM STMWI_CLIPON}
  STMWI_CLIPON     = 4; // Microphone clips onto shirt
  {$EXTERNALSYM STMWI_DESKTOP}
  STMWI_DESKTOP    = 5; // Microphone sits on desktop
  {$EXTERNALSYM STMWI_HANDHELD}
  STMWI_HANDHELD   = 6; // Microphone that is meant to be held in the user's hand a few cm from his/her face
  {$EXTERNALSYM STMWI_TOPMONITOR}
  STMWI_TOPMONITOR = 7; // Microphone that sits on top of the computer monitor
  {$EXTERNALSYM STMWI_INMONITOR}
  STMWI_INMONITOR  = 8; // Microphone built into the monitor
  {$EXTERNALSYM STMWI_KEYBOARD}
  STMWI_KEYBOARD   = 9; // Microphone built into the keyboard
  {$EXTERNALSYM STMWI_REMOTE}
  STMWI_REMOTE     = 10; // Microphone in a remote control!

  {$EXTERNALSYM STMWIS_UNKNOWN}
  STMWIS_UNKNOWN   =   0;
  {$EXTERNALSYM STMWIS_SPEAKERS}
  STMWIS_SPEAKERS  =   1;
  {$EXTERNALSYM STMWIS_HEADPHONES}
  STMWIS_HEADPHONES =   2;
  {$EXTERNALSYM STMWIS_BOTH}
  STMWIS_BOTH      =   3;

type

// Speech Tools - Mic Wizard 
  PMicWizardInfo = ^TMicWizardInfo;
  {$EXTERNALSYM MICWIZARDINFO}
  MICWIZARDINFO = record
    fHasRunMicWizard: BOOL;
    dwWaveInDevice: DWORD;
    dwMicrophoneType: DWORD;
    szMicString: Array[0..63] of WideChar;
  end;
  TMicWizardInfo = MICWIZARDINFO;

  PMicWizardInfo2 = ^TMicWizardInfo2;
  {$EXTERNALSYM MICWIZARDINFO2}
  MICWIZARDINFO2 = record
    fHasRunMicWizard: BOOL;
    dwWaveInDevice: DWORD;
    dwMicrophoneType: DWORD;
    szMicString: Array[0..63] of WideChar;
    dwSpeakers: DWORD;
    dwLastMicLevel: DWORD;
    dwMicLevelDeviceID: DWORD;
  end;
  TMicWizardInfo2 = MICWIZARDINFO2;

const
// STLexDlg
  {$EXTERNALSYM STLD_DISABLEREMOVE}
  STLD_DISABLEREMOVE         = $00000001 ;                //SAPI 4.0
  {$EXTERNALSYM STLD_DISABLEADD}
  STLD_DISABLEADD            = $00000002 ;                //SAPI 4.0
  {$EXTERNALSYM STLD_FORCEEDIT}
  STLD_FORCEEDIT             = $00000004 ;                //SAPI 4.0
  {$EXTERNALSYM STLD_DISABLEPRONADDREMOVE}
  STLD_DISABLEPRONADDREMOVE  = $00000008 ;                //SAPI 4.0
  {$EXTERNALSYM STLD_TEST}
  STLD_TEST                  = $00000010 ;                //SAPI 4.0
  {$EXTERNALSYM STLD_DISABLERENAME}
  STLD_DISABLERENAME         = $00000020 ;                //SAPI 4.0
  {$EXTERNALSYM STLD_CHANGEPRONADDS}
  STLD_CHANGEPRONADDS        = $00000040 ;                //SAPI 4.0

type
  {$EXTERNALSYM ISTMicWizard}
  ISTMicWizard = interface(IUnknown)
     [SID_ISTMicWizard]
     function InfoGet(pInfo: PMICWIZARDINFO): HResult; stdcall;
     function InfoSet(pInfo: PMICWIZARDINFO): HResult; stdcall;
     function Wizard(hWndParent: HWND; dwUse, dwWaveInDevice,
       dwSamplesPerSec, dwFlags: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTMICWIZARD}
  PISTMICWIZARD = ^ISTMicWizard;


  {$EXTERNALSYM ISTLexDlg}
  ISTLexDlg = interface(IUnknown)
    [SID_ISTLexDlg]
    function LexDlg(hWnd: HWND; pszWord: LPCWSTR; dwFlags: DWORD;
      LangID: LANGID; UnkLex: IUnknown; UnkTTS: IUnknown;
      UnkBackupLex: IUnknown; pszPhonemes: LPCWSTR;
      UnDocumented: LPCWSTR): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTLEXDLG}
  PISTLEXDLG = ^ISTLexDlg;

  {This only applies for Unicode compiles }
  {$EXTERNALSYM ISTPhoneConv}
  ISTPhoneConv = interface(IUnknown)
    [SID_ISTPhoneConv]
    function FromIPA(pszFromIPA: LPCWSTR; var dwFirstInvalid: DWORD;
      pszToSet: LPWSTR; dwNumChars: DWORD; var dwCharsNeeded: DWORD): HResult; stdcall;
    function Get(var Data: SDATA): HResult; stdcall;
    function SetByLang(Lang: LANGID): HResult; stdcall;
    function SetByText(pszText: LPCWSTR): HResult; stdcall;
    function ToIPA(pszFromSet: LPCWSTR; var dwFirstInvalid: DWORD;
      pszToIPA: LPWSTR; dwNumChars: DWORD; var dwCharsNeeded: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTPHONECONV}
  PISTPHONECONV = ^ISTPhoneConv;

  // Audio source/destiantion APIs
  // Audio source/destiantion API Constants
const
  // Audio Stop
  {$EXTERNALSYM IANSRSN_NODATA}
  IANSRSN_NODATA    = 0;
  {$EXTERNALSYM IANSRSN_PRIORITY}
  IANSRSN_PRIORITY  = 1;
  {$EXTERNALSYM IANSRSN_INACTIVE}
  IANSRSN_INACTIVE  = 2;
  {$EXTERNALSYM IANSRSN_EOF}
  IANSRSN_EOF       = 3;

  // IAudioSourceInstrumented::StateSet

  {$EXTERNALSYM IASISTATE_PASSTHROUGH}
  IASISTATE_PASSTHROUGH      = 0;   {SAPI 3}
  {$EXTERNALSYM IASISTATE_PASSNOTHING}
  IASISTATE_PASSNOTHING      = 1;   {SAPI 3}
  {$EXTERNALSYM IASISTATE_PASSREADFROMWAVE}
  IASISTATE_PASSREADFROMWAVE = 2;   {SAPI 3}
  {$EXTERNALSYM IASISTATE_PASSWRITETOWAVE}
  IASISTATE_PASSWRITETOWAVE  = 3;   {SAPI 3}

type
  {$EXTERNALSYM IAudio}
  IAudio = interface(IUnknown)
    [SID_IAudio]
    function Flush: HResult; stdcall;
    function LevelGet(var dwLevel: DWORD): HResult; stdcall;
    function LevelSet(dwLevel: DWORD): HResult; stdcall;
    function PassNotify(pNotifyInterface: Pointer; IIDNotifyInterface: TIID): HResult; stdcall;
    function PosnGet(var qwTimeStamp: QWORD): HResult; stdcall;
    function Claim: HResult; stdcall;
    function UnClaim: HResult; stdcall;
    function Start: HResult; stdcall;
    function Stop: HResult; stdcall;
    function TotalGet(var qWord: QWORD): HResult; stdcall;
    function ToFileTime(var qWord: QWORD; var FT: TFileTime): HResult; stdcall;
    function WaveFormatGet(var dWFEX: SDATA): HResult; stdcall;
    function WaveFormatSet(dWFEX: SDATA): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIO}
  PIAUDIO = ^IAudio;

  {$EXTERNALSYM IAudioDest}
  IAudioDest = interface(IUnknown)
    [SID_IAudioDest]
    function FreeSpace(var dwBytes: DWORD; var fEOF: BOOL): HResult; stdcall;
    function DataSet(pBuffer: Pointer; dwSize: DWORD): HResult; stdcall;
    function BookMark(dwMarkID: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIODEST}
  PIAUDIODEST = ^IAudioDest;

  {$EXTERNALSYM IAudioDestNotifySink}
  IAudioDestNotifySink = interface(IUnknown)
    [SID_IAudioDestNotifySink]
    function AudioStop(wReason: WORD): HResult; stdcall;
    function AudioStart: HResult; stdcall;
    function FreeSpace(dwBytes: DWORD; fEOF: BOOL): HResult; stdcall;
    function BookMark(dwMarkID: DWORD; fFlush: BOOL): HResult; stdcall;
  end; //IAudioDestNotifySink
  {$EXTERNALSYM PIAUDIODESTNOTIFYSINK}
  PIAUDIODESTNOTIFYSINK = ^IAudioDestNotifySink;

  {$EXTERNALSYM IAudioMultiMediaDevice}
  IAudioMultiMediaDevice = interface(IUnknown)
    [SID_IAudioMultiMediaDevice]
    function CustomMessage(uMsg: UINT; dData: SDATA): HResult; stdcall;
    function DeviceNumGet(var dwDeviceID: DWORD): HResult; stdcall;
    function DeviceNumSet(dwDeviceID: DWORD): HResult; stdcall;
  end; // IAudioMultiMediaDevice
  {$EXTERNALSYM PIAUDIOMULTIMEDIADEVICE}
  PIAUDIOMULTIMEDIADEVICE = ^IAudioMultiMediaDevice;

  {$EXTERNALSYM IAudioSource}
  IAudioSource = interface(IUnknown)
    [SID_IAudioSource]
    function DataAvailable(var dwBytes: DWORD; var fEOF: BOOL): HResult; stdcall;
    function DataGet(pBuffer: Pointer; dwGetSize: DWORD;
      var dwCopied: DWORD): HResult; stdcall;
  end; // IAudioSource
  {$EXTERNALSYM PIAUDIOSOURCE}
  PIAUDIOSOURCE = ^IAudioSource;

  // Next is unicode only
  {$EXTERNALSYM IAudioSourceInstrumented}
  IAudioSourceInstrumented = interface(IUnknown)
    [SID_IAudioSourceInstrumented]
    function AudioSource(aAudioSource: IUnknown): HResult; stdcall;
    function RegistryGet(pszKey: LPWSTR; dwKeySize: DWORD; var dwNeeded: DWORD): HResult; stdcall;
    function RegistrySet(pszKey: LPCWSTR): HResult; stdcall;
    function StateGet(var dwState: DWORD): HResult; stdcall;
    function StateSet(dwState: DWORD): HResult; stdcall;
    function WaveFileReadGet(pszFileName: LPWSTR; dwFileNameSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function WaveFileReadSet(pszWaveFile: LPCWSTR): HResult; stdcall;
    function WaveFileWriteGet(pszFileName: LPWSTR; dwFileNameSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function WaveFileWriteSet(pszWaveFile: LPCWSTR): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOSOURCEINSTRUMENTED}
  PIAUDIOSOURCEINSTRUMENTED = ^IAudioSourceInstrumented;

  {$EXTERNALSYM IAudioSourceSusp}
  IAudioSourceSusp = interface(IUnknown)
    [SID_IAudioSourceSusp]
    function AudioSource(aAudioSource: IUnknown): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOSOURCESUSP}
  PIAUDIOSOURCESUSP = ^IAudioSourceSusp;

  {$EXTERNALSYM IAudioSourceNotifySink}
  IAudioSourceNotifySink = interface(IUnknown)
    [SID_IAudioSourceNotifySink]
    function AudioStop(wReason: WORD): HResult; stdcall;
    function AudioStart: HResult; stdcall;
    function DataAvailable(dwBytes: DWORD; fEOF: BOOL): HResult; stdcall;
    function Overflow(dwBytes: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOSOURCENOTIFYSINK}
  PIAUDIOSOURCENOTIFYSINK =^IAudioSourceNotifySink;

  {$EXTERNALSYM IAudioFileNotifySink}
  IAudioFileNotifySink = interface(IUnknown)
    [SID_IAudioFileNotifySink]
    function FileBegin(dwID: DWORD): HResult; stdcall;
    function FileEnd(dwID: DWORD): HResult; stdcall;
    function QueueEmpty: HResult; stdcall;
    function Posn(qwProcessed: QWORD; qwLeft: QWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOFILENOTIFYSINK}
  PIAUDIOFILENOTIFYSINK =^IAudioFileNotifySink;

  {$EXTERNALSYM IAudioFile}
  IAudioFile = interface(IUnknown) //Unicode only
    [SID_IAudioFile]
    function Register(NotifyInterface: IAudioFileNotifySink): HResult; stdcall;
    function DoSet(pszFile: LPCWSTR; dwID: DWORD): HResult; stdcall;
    function Add(pszFile: LPCWSTR; dwID: DWORD): HResult; stdcall;
    function Flush: HResult; stdcall;
    function RealTimeSet(wRealTime: WORD): HResult; stdcall;
    function RealTimeGet(var wRealTime: WORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOFILE}
  PIAUDIOFILE =^ IAudioFile;

  {$EXTERNALSYM IAudioDirect}
  IAudioDirect = interface(IUnknown)
    [SID_IAudioDirect]
    function Init(pInterface: Pointer; iid: TGUID): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIODIRECT}
  PIAUDIODIRECT = ^IAudioDirect;

//************************************************************************
// Low-Level Speach Recognitions API Constants
//************************************************************************
const
  {$EXTERNALSYM SRMI_NAMELEN}
  SRMI_NAMELEN                     = SVFN_LEN;
  {$EXTERNALSYM SRSEQUENCE_DISCRETE}
  SRSEQUENCE_DISCRETE              = 0;
  {$EXTERNALSYM SRSEQUENCE_CONTINUOUS}
  SRSEQUENCE_CONTINUOUS            = 1;
  {$EXTERNALSYM SRSEQUENCE_WORDSPOT}
  SRSEQUENCE_WORDSPOT              = 2;
  {$EXTERNALSYM SRSEQUENCE_CONTCFGDISCDICT}
  SRSEQUENCE_CONTCFGDISCDICT       = 3; {SAPI 3}

  // Engine Grammar types 
  {$EXTERNALSYM SRGRAM_CFG}
  SRGRAM_CFG                       = $00000001 ;     //{bit 0}
  {$EXTERNALSYM SRGRAM_DICTATION}
  SRGRAM_DICTATION                 = $00000002 ;     //{bit 1}
  {$EXTERNALSYM SRGRAM_LIMITEDDOMAIN}
  SRGRAM_LIMITEDDOMAIN             = $00000004 ;     //{bit 2}

  // SR Engine Features 
  {$EXTERNALSYM SRFEATURE_INDEPSPEAKER}
  SRFEATURE_INDEPSPEAKER           = $00000001 ;     //{bit 0}
  {$EXTERNALSYM SRFEATURE_INDEPMICROPHONE}
  SRFEATURE_INDEPMICROPHONE        = $00000002 ;     //{bit 1}
  {$EXTERNALSYM SRFEATURE_TRAINWORD}
  SRFEATURE_TRAINWORD              = $00000004 ;     //{bit 2}
  {$EXTERNALSYM SRFEATURE_TRAINPHONETIC}
  SRFEATURE_TRAINPHONETIC          = $00000008 ;     //{bit 3}
  {$EXTERNALSYM SRFEATURE_WILDCARD}
  SRFEATURE_WILDCARD               = $00000010 ;     //{bit 4}
  {$EXTERNALSYM SRFEATURE_ANYWORD}
  SRFEATURE_ANYWORD                = $00000020 ;     //{bit 5}
  {$EXTERNALSYM SRFEATURE_PCOPTIMIZED}
  SRFEATURE_PCOPTIMIZED            = $00000040 ;     //{bit 6}
  {$EXTERNALSYM SRFEATURE_PHONEOPTIMIZED}
  SRFEATURE_PHONEOPTIMIZED         = $00000080 ;     //{bit 7}
  {$EXTERNALSYM SRFEATURE_GRAMLIST}
  SRFEATURE_GRAMLIST               = $00000100 ;     //{bit 8}
  {$EXTERNALSYM SRFEATURE_GRAMLINK}
  SRFEATURE_GRAMLINK               = $00000200 ;     //{bit 9}
  {$EXTERNALSYM SRFEATURE_MULTILINGUAL}
  SRFEATURE_MULTILINGUAL           = $00000400 ;     //{bit 10}
  {$EXTERNALSYM SRFEATURE_GRAMRECURSIVE}
  SRFEATURE_GRAMRECURSIVE          = $00000800 ;     //{bit 11}
  {$EXTERNALSYM SRFEATURE_IPAUNICODE}
  SRFEATURE_IPAUNICODE             = $00001000 ;     //{bit 12}
  {$EXTERNALSYM SRFEATURE_SINGLEINSTANCE}
  SRFEATURE_SINGLEINSTANCE         = $00002000 ;     //{bit 13}//SAPI 4
  {$EXTERNALSYM SRFEATURE_THREADSAFE}
  SRFEATURE_THREADSAFE             = $00004000 ;     //{bit 14}//SAPI 4
  {$EXTERNALSYM SRFEATURE_FIXEDAUDIO}
  SRFEATURE_FIXEDAUDIO             = $00008000 ;     //{bit 15}//SAPI 4
  {$EXTERNALSYM SRFEATURE_IPAWORD}
  SRFEATURE_IPAWORD                = $00010000 ;     //{bit 16}//SAPI 4
  {$EXTERNALSYM SRFEATURE_SAPI4}
  SRFEATURE_SAPI4                  = $00020000 ;     //{bit 17}//SAPI 4

  // SR Engine Interfaces Supported 
  {$EXTERNALSYM SRI_ILEXPRONOUNCE}
  SRI_ILEXPRONOUNCE                = $00000001 ;     //{bit 0}
  {$EXTERNALSYM SRI_ISRATTRIBUTES}
  SRI_ISRATTRIBUTES                = $00000002 ;     //{bit 1}
  {$EXTERNALSYM SRI_ISRCENTRAL}
  SRI_ISRCENTRAL                   = $00000004 ;     //{bit 2}
  {$EXTERNALSYM SRI_ISRDIALOGS}
  SRI_ISRDIALOGS                   = $00000008 ;     //{bit 3}
  {$EXTERNALSYM SRI_ISRGRAMCOMMON}
  SRI_ISRGRAMCOMMON                = $00000010 ;     //{bit 4}
  {$EXTERNALSYM SRI_ISRGRAMCFG}
  SRI_ISRGRAMCFG                   = $00000020 ;     //{bit 5}
  {$EXTERNALSYM SRI_ISRGRAMDICTATION}
  SRI_ISRGRAMDICTATION             = $00000040 ;     //{bit 6}
  {$EXTERNALSYM SRI_ISRGRAMINSERTIONGUI}
  SRI_ISRGRAMINSERTIONGUI          = $00000080 ;     //{bit 7}
  {$EXTERNALSYM SRI_ISRESBASIC}
  SRI_ISRESBASIC                   = $00000100 ;     //{bit 8}
  {$EXTERNALSYM SRI_ISRESMERGE}
  SRI_ISRESMERGE                   = $00000200 ;     //{bit 9}
  {$EXTERNALSYM SRI_ISRESAUDIO}
  SRI_ISRESAUDIO                   = $00000400 ;     //{bit 10}
  {$EXTERNALSYM SRI_ISRESCORRECTION}
  SRI_ISRESCORRECTION              = $00000800 ;     //{bit 11}
  {$EXTERNALSYM SRI_ISRESEVAL}
  SRI_ISRESEVAL                    = $00001000 ;     //{bit 12}
  {$EXTERNALSYM SRI_ISRESGRAPH}
  SRI_ISRESGRAPH                   = $00002000 ;     //{bit 13}
  {$EXTERNALSYM SRI_ISRESMEMORY}
  SRI_ISRESMEMORY                  = $00004000 ;     //{bit 14}
  {$EXTERNALSYM SRI_ISRESMODIFYGUI}
  SRI_ISRESMODIFYGUI               = $00008000 ;     //{bit 15}
  {$EXTERNALSYM SRI_ISRESSPEAKER}
  SRI_ISRESSPEAKER                 = $00010000 ;     //{bit 16}
  {$EXTERNALSYM SRI_ISRSPEAKER}
  SRI_ISRSPEAKER                   = $00020000 ;     //{bit 17}
  {$EXTERNALSYM SRI_ISRESSCORES}
  SRI_ISRESSCORES                  = $00040000 ;     //{bit 18} {SAPI 3}
  {$EXTERNALSYM SRI_ISRESAUDIOEX}
  SRI_ISRESAUDIOEX                 = $00080000 ;     //{bit 19} {SAPI 4}
  {$EXTERNALSYM SRI_ISRGRAMLEXPRON}
  SRI_ISRGRAMLEXPRON               = $00100000 ;     //{bit 20} {SAPI 4}
  {$EXTERNALSYM SRI_ISRRESGRAPHEX}
  SRI_ISRRESGRAPHEX                = $00200000 ;     //{bit 21} {SAPI 4}
  {$EXTERNALSYM SRI_ILEXPRONOUNCE2}
  SRI_ILEXPRONOUNCE2               = $00400000 ;     //{bit 22} {SAPI 4}
  {$EXTERNALSYM SRI_IATTRIBUTES}
  SRI_IATTRIBUTES                  = $00800000 ;     //{bit 23} {SAPI 4}
  {$EXTERNALSYM SRI_ISRSPEAKER2}
  SRI_ISRSPEAKER2                  = $01000000 ;     //{bit 24} {SAPI 4}
  {$EXTERNALSYM SRI_ISRDIALOGS2}
  SRI_ISRDIALOGS2                  = $02000000 ;     //{bit 25} {SAPI 4}

  // For ISRGramCommon.TrainQuery 
  {$EXTERNALSYM SRGRAMQ_NONE}
  SRGRAMQ_NONE                     = 0;
  {$EXTERNALSYM SRGRAMQ_GENERALTRAIN}
  SRGRAMQ_GENERALTRAIN             = 1;
  {$EXTERNALSYM SRGRAMQ_PHRASE}
  SRGRAMQ_PHRASE                   = 2;
  {$EXTERNALSYM SRGRAMQ_DIALOG}
  SRGRAMQ_DIALOG                   = 3;

  // For ISRGramNotifySink.PhraseFinish 
  {$EXTERNALSYM ISRNOTEFIN_RECOGNIZED}
  ISRNOTEFIN_RECOGNIZED            = $00000001 ;     //{bit 0}
  {$EXTERNALSYM ISRNOTEFIN_THISGRAMMAR}
  ISRNOTEFIN_THISGRAMMAR           = $00000002 ;     //{bit 1}
  {$EXTERNALSYM ISRNOTEFIN_FROMTHISGRAMMAR}
  ISRNOTEFIN_FROMTHISGRAMMAR       = $00000004 ;     //{bit 2} {SAPI 3)

  // For ISRGramNotifySink.Training 
  {$EXTERNALSYM SRGNSTRAIN_GENERAL}
  SRGNSTRAIN_GENERAL               = $00000001 ;     //{bit 0}
  {$EXTERNALSYM SRGNSTRAIN_GRAMMAR}
  SRGNSTRAIN_GRAMMAR               = $00000002 ;     //{bit 1}
  {$EXTERNALSYM SRGNSTRAIN_MICROPHONE}
  SRGNSTRAIN_MICROPHONE            = $00000004 ;     //{bit 2}

  // For ISRNotifySink::AttribChange 
  {$EXTERNALSYM ISRNSAC_AUTOGAINENABLE}
  ISRNSAC_AUTOGAINENABLE           = 1;
  {$EXTERNALSYM ISRNSAC_THRESHOLD}
  ISRNSAC_THRESHOLD                = 2;
  {$EXTERNALSYM ISRNSAC_ECHO}
  ISRNSAC_ECHO                     = 3;
  {$EXTERNALSYM ISRNSAC_ENERGYFLOOR}
  ISRNSAC_ENERGYFLOOR              = 4;
  {$EXTERNALSYM ISRNSAC_MICROPHONE}
  ISRNSAC_MICROPHONE               = 5;
  {$EXTERNALSYM ISRNSAC_REALTIME}
  ISRNSAC_REALTIME                 = 6;
  {$EXTERNALSYM ISRNSAC_SPEAKER}
  ISRNSAC_SPEAKER                  = 7;
  {$EXTERNALSYM ISRNSAC_TIMEOUT}
  ISRNSAC_TIMEOUT                  = 8;
  {$EXTERNALSYM ISRNSAC_STARTLISTENING}
  ISRNSAC_STARTLISTENING           = 9;   //SAPI 4
  {$EXTERNALSYM ISRNSAC_STOPLISTENING}
  ISRNSAC_STOPLISTENING            = 10;  //SAPI 4

  // Interference 
  {$EXTERNALSYM SRMSGINT_NOISE}
  SRMSGINT_NOISE                   = $0001;
  {$EXTERNALSYM SRMSGINT_NOSIGNAL}
  SRMSGINT_NOSIGNAL                = $0002;
  {$EXTERNALSYM SRMSGINT_TOOLOUD}
  SRMSGINT_TOOLOUD                 = $0003;
  {$EXTERNALSYM SRMSGINT_TOOQUIET}
  SRMSGINT_TOOQUIET                = $0004;
  {$EXTERNALSYM SRMSGINT_AUDIODATA_STOPPED}
  SRMSGINT_AUDIODATA_STOPPED       = $0005;
  {$EXTERNALSYM SRMSGINT_AUDIODATA_STARTED}
  SRMSGINT_AUDIODATA_STARTED       = $0006;
  {$EXTERNALSYM SRMSGINT_IAUDIO_STARTED}
  SRMSGINT_IAUDIO_STARTED          = $0007;
  {$EXTERNALSYM SRMSGINT_IAUDIO_STOPPED}
  SRMSGINT_IAUDIO_STOPPED          = $0008;

  // Gramamr header values
  {$EXTERNALSYM SRHDRTYPE_CFG}
  SRHDRTYPE_CFG                    = 0;
  {$EXTERNALSYM SRHDRTYPE_LIMITEDDOMAIN}
  SRHDRTYPE_LIMITEDDOMAIN          = 1;
  {$EXTERNALSYM SRHDRTYPE_DICTATION}
  SRHDRTYPE_DICTATION              = 2;

  {$EXTERNALSYM SRHDRFLAG_UNICODE}
  SRHDRFLAG_UNICODE                = $00000001 ;     //{bit 0}

  // results objects defines
  {$EXTERNALSYM SRRESCUE_COMMA}
  SRRESCUE_COMMA                   = 1; //SAPI 4
  {$EXTERNALSYM SRRESCUE_DECLARATIVEBEGIN}
  SRRESCUE_DECLARATIVEBEGIN        = 2; //SAPI 4
  {$EXTERNALSYM SRRESCUE_DECLARATIVEEND}
  SRRESCUE_DECLARATIVEEND          = 3; //SAPI 4
  {$EXTERNALSYM SRRESCUE_IMPERATIVEBEGIN}
  SRRESCUE_IMPERATIVEBEGIN         = 4; //SAPI 4
  {$EXTERNALSYM SRRESCUE_IMPERATIVEEND}
  SRRESCUE_IMPERATIVEEND           = 5; //SAPI 4
  {$EXTERNALSYM SRRESCUE_INTERROGATIVEBEGIN}
  SRRESCUE_INTERROGATIVEBEGIN      = 6; //SAPI 4
  {$EXTERNALSYM SRRESCUE_INTERROGATIVEEND}
  SRRESCUE_INTERROGATIVEEND        = 7; //SAPI 4
  {$EXTERNALSYM SRRESCUE_NOISE}
  SRRESCUE_NOISE                   = 8; //SAPI 4
  {$EXTERNALSYM SRRESCUE_PAUSE}
  SRRESCUE_PAUSE                   = 9; //SAPI 4
  {$EXTERNALSYM SRRESCUE_SENTENCEBEGIN}
  SRRESCUE_SENTENCEBEGIN           = 10; //SAPI 4
  {$EXTERNALSYM SRRESCUE_SENTENCEEND}
  SRRESCUE_SENTENCEEND             = 11; //SAPI 4
  {$EXTERNALSYM SRRESCUE_UM}
  SRRESCUE_UM                      = 12; //SAPI 4
  {$EXTERNALSYM SRRESCUE_WILDCARD}
  SRRESCUE_WILDCARD                = 13; //SAPI 4
  {$EXTERNALSYM SRRESCUE_WORD}
  SRRESCUE_WORD                    = 14; //SAPI 4

  // SRCFGSYMBOL 
  {$EXTERNALSYM SRCFG_STARTOPERATION}
  SRCFG_STARTOPERATION             = 1;
  {$EXTERNALSYM SRCFG_ENDOPERATION}
  SRCFG_ENDOPERATION               = 2;
  {$EXTERNALSYM SRCFG_WORD}
  SRCFG_WORD                       = 3;
  {$EXTERNALSYM SRCFG_RULE}
  SRCFG_RULE                       = 4;
  {$EXTERNALSYM SRCFG_WILDCARD}
  SRCFG_WILDCARD                   = 5;
  {$EXTERNALSYM SRCFG_LIST}
  SRCFG_LIST                       = 6;

  {$EXTERNALSYM SRCFGO_SEQUENCE}
  SRCFGO_SEQUENCE                  = 1;
  {$EXTERNALSYM SRCFGO_ALTERNATIVE}
  SRCFGO_ALTERNATIVE               = 2;
  {$EXTERNALSYM SRCFGO_REPEAT}
  SRCFGO_REPEAT                    = 3;
  {$EXTERNALSYM SRCFGO_OPTIONAL}
  SRCFGO_OPTIONAL                  = 4;

  // Grammar-chunk IDs 
  {$EXTERNALSYM SRCK_LANGUAGE}
  SRCK_LANGUAGE                    = 1;
  {$EXTERNALSYM SRCKCFG_WORDS}
  SRCKCFG_WORDS                    = 2;
  {$EXTERNALSYM SRCKCFG_RULES}
  SRCKCFG_RULES                    = 3;
  {$EXTERNALSYM SRCKCFG_EXPORTRULES}
  SRCKCFG_EXPORTRULES              = 4;
  {$EXTERNALSYM SRCKCFG_IMPORTRULES}
  SRCKCFG_IMPORTRULES              = 5;
  {$EXTERNALSYM SRCKCFG_LISTS}
  SRCKCFG_LISTS                    = 6;
  {$EXTERNALSYM SRCKD_TOPIC}
  SRCKD_TOPIC                      = 7;
  {$EXTERNALSYM SRCKD_COMMON}
  SRCKD_COMMON                     = 8;
  {$EXTERNALSYM SRCKD_GROUP}
  SRCKD_GROUP                      = 9;
  {$EXTERNALSYM SRCKD_SAMPLE}
  SRCKD_SAMPLE                     = 10;
  {$EXTERNALSYM SRCKLD_WORDS}
  SRCKLD_WORDS                     = 11;
  {$EXTERNALSYM SRCKLD_GROUP}
  SRCKLD_GROUP                     = 12;
  {$EXTERNALSYM SRCKLD_SAMPLE}
  SRCKLD_SAMPLE                    = 13;
  {$EXTERNALSYM SRCKD_WORDCOUNT}
  SRCKD_WORDCOUNT                  = 14; //{SAPI 3}
  {$EXTERNALSYM SRCKD_NGRAM}
  SRCKD_NGRAM                      = 15; //{SAPI 3}

  // TrainQuery 
  {$EXTERNALSYM SRTQEX_REQUIRED}
  SRTQEX_REQUIRED                  = $0000;
  {$EXTERNALSYM SRTQEX_RECOMMENDED}
  SRTQEX_RECOMMENDED               = $0001;

  // ISRResAudioEx
  {$EXTERNALSYM SRAUDIOTIMESTAMP_DEFAULT}
  SRAUDIOTIMESTAMP_DEFAULT         = High(QWORD)-1; // SAPI 4 ((QWORD) -1)

  // ISRResCorrection 
  {$EXTERNALSYM SRCORCONFIDENCE_SOME}
  SRCORCONFIDENCE_SOME             = $0001;
  {$EXTERNALSYM SRCORCONFIDENCE_VERY}
  SRCORCONFIDENCE_VERY             = $0002;

  // ISRResGraphEx                 SAPI 4
  {$EXTERNALSYM SRGARC_ACOUSTICSCORE}
  SRGARC_ACOUSTICSCORE: TGUID = (
    D1:$bf2f5b20; D2:$b91d; D3:$11d1; D4:($b7,$bc,$00,$60,$97,$98,$41,$b4));
  {$EXTERNALSYM SRGARC_WORDTEXT}
  SRGARC_WORDTEXT: TGUID = (
    D1:$bf2f5b21; D2:$b91d; D3:$11d1; D4:($b7,$bc,$00,$60,$97,$98,$41,$b4));
  {$EXTERNALSYM SRGARC_NONLEXICALTEXT}
  SRGARC_NONLEXICALTEXT: TGUID = (
    D1:$0062ec00; D2:$baa0; D3:$11d1; D4:($b7,$bd,$00,$60,$97,$98,$41,$b4));
  {$EXTERNALSYM SRGARC_WORDPRONUNCIATION}
  SRGARC_WORDPRONUNCIATION: TGUID = (
    D1:$bf2f5b22; D2:$b91d; D3:$11d1; D4:($b7,$bc,$00,$60,$97,$98,$41,$b4));
  {$EXTERNALSYM SRGNODE_TIME}
  SRGNODE_TIME: TGUID = (
    D1:$bf2f5b23; D2:$b91d; D3:$11d1; D4:($b7,$bc,$00,$60,$97,$98,$41,$b4));

  {$EXTERNALSYM SRGEX_ACOUSTICONLY}
  SRGEX_ACOUSTICONLY            = $00000001 ;     {bit 0}  //SAPI 4
  {$EXTERNALSYM SRGEX_LMONLY}
  SRGEX_LMONLY                  = $00000002 ;     {bit 1}  //SAPI 4
  {$EXTERNALSYM SRGEX_ACOUSTICANDLM}
  SRGEX_ACOUSTICANDLM           = $00000004 ;     {bit 2}  //SAPI 4

  // ISRResMemory constants 
  {$EXTERNALSYM SRRESMEMKIND_AUDIO}
  SRRESMEMKIND_AUDIO               = $00000001 ;     {bit 0}
  {$EXTERNALSYM SRRESMEMKIND_CORRECTION}
  SRRESMEMKIND_CORRECTION          = $00000002 ;     {bit 1}
  {$EXTERNALSYM SRRESMEMKIND_EVAL}
  SRRESMEMKIND_EVAL                = $00000004 ;     {bit 2}
  {$EXTERNALSYM SRRESMEMKIND_PHONEMEGRAPH}
  SRRESMEMKIND_PHONEMEGRAPH        = $00000008 ;     {bit 3}
  {$EXTERNALSYM SRRESMEMKIND_WORDGRAPH}
  SRRESMEMKIND_WORDGRAPH           = $00000010 ;     {bit 4}

  // Attribute minimums and maximums
  {$EXTERNALSYM SRATTR_MINAUTOGAIN}
  SRATTR_MINAUTOGAIN               = 0;
  {$EXTERNALSYM SRATTR_MAXAUTOGAIN}
  SRATTR_MAXAUTOGAIN               = 100;
  {$EXTERNALSYM SRATTR_MINENERGYFLOOR}
  SRATTR_MINENERGYFLOOR            = 0;
  {$EXTERNALSYM SRATTR_MAXENERGYFLOOR}
  SRATTR_MAXENERGYFLOOR            = $ffff;
  {$EXTERNALSYM SRATTR_MINREALTIME}
  SRATTR_MINREALTIME               = 0;
  {$EXTERNALSYM SRATTR_MAXREALTIME}
  SRATTR_MAXREALTIME               = DWORD($ffffffff);
  {$EXTERNALSYM SRATTR_MINTHRESHOLD}
  SRATTR_MINTHRESHOLD              = 0;
  {$EXTERNALSYM SRATTR_MAXTHRESHOLD}
  SRATTR_MAXTHRESHOLD              = 100;
  {$EXTERNALSYM SRATTR_MINTOINCOMPLETE}
  SRATTR_MINTOINCOMPLETE           = 0;
  {$EXTERNALSYM SRATTR_MAXTOINCOMPLETE}
  SRATTR_MAXTOINCOMPLETE           = DWORD($ffffffff);
  {$EXTERNALSYM SRATTR_MINTOCOMPLETE}
  SRATTR_MINTOCOMPLETE             = 0;
  {$EXTERNALSYM SRATTR_MAXTOCOMPLETE}
  SRATTR_MAXTOCOMPLETE             = DWORD($ffffffff);

// Low-Level Speach Recognitions
type
  PSRCFGRule = ^TSRCFGRule;
  {$EXTERNALSYM SRCFGRULE}
  SRCFGRULE = record
    dwSize:      DWORD;
    dwUniqueID:  DWORD;
    abData : record end;
    //abData: array[0..0] of BYTE;  //This is a reminder that data is appended
  end;
  TSRCFGRule = SRCFGRULE;

  PSRCFGImpRuleA = ^TSRCFGImpRuleA;
  PSRCFGImpRuleW = ^TSRCFGImpRuleW;
  PSRCFGImpRule = PSRCFGImpRuleA;
  {$EXTERNALSYM SRCFGIMPRULEA}
  SRCFGIMPRULEA = record
    dwSize:     DWORD;
    dwRuleNum:  DWORD;
    szString : record end;
    //szString: array[0..0] of AnsiChar;  //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRCFGIMPRULEW}
  SRCFGIMPRULEW = record
    dwSize:     DWORD;
    dwRuleNum:  DWORD;
    szString : record end;
    //szString: array[0..0] of WideChar;  //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRCFGIMPRULE}
  SRCFGIMPRULE = SRCFGIMPRULEA;
  TSRCFGImpRuleA = SRCFGIMPRULEA;
  TSRCFGImpRuleW = SRCFGIMPRULEW;
  TSRCFGImpRule = TSRCFGImpRuleA;


  PSRCFGXRuleA = ^TSRCFGXRuleA;
  PSRCFGXRuleW = ^TSRCFGXRuleW;
  PSRCFGXRule = PSRCFGXRuleA;
  {$EXTERNALSYM SRCFGXRULEA}
  SRCFGXRULEA = record
    dwSize:      DWORD;
    dwRuleNum:   DWORD;
    szString : record end;
    //szString: array[0..0] of AnsiChar;  //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRCFGXRULEW}
  SRCFGXRULEW = record
    dwSize:      DWORD;
    dwRuleNum:   DWORD;
    szString : record end;
    //szString: array[0..0] of WideChar;  //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRCFGXRULE}
  SRCFGXRULE = SRCFGXRULEA;
  TSRCFGXRuleA = SRCFGXRULEA;
  TSRCFGXRuleW = SRCFGXRULEW;
  TSRCFGXRule = TSRCFGXRuleA;


  PSRCFGListA = ^TSRCFGListA;
  PSRCFGListW = ^TSRCFGListW;
  PSRCFGList = PSRCFGListA;
  {$EXTERNALSYM SRCFGLISTA}
  SRCFGLISTA = record
    dwSize:      DWORD;
    dwListNum:   DWORD;
    szString : record end;
    //szString: array[0..0] of AnsiChar;  //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRCFGLISTW}
  SRCFGLISTW = record
    dwSize:      DWORD;
    dwListNum:   DWORD;
    szString : record end;
    //szString: array[0..0] of WideChar;  //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRCFGLIST}
  SRCFGLIST = SRCFGLISTA;
  TSRCFGListA = SRCFGLISTA;
  TSRCFGListW = SRCFGLISTW;
  TSRCFGList = TSRCFGListA;

  PSRCFGSymbol = ^TSRCFGSymbol;
  {$EXTERNALSYM SRCFGSYMBOL}
  SRCFGSYMBOL = record
    wType:        WORD;
    wProbability: WORD;
    dwValue:      DWORD;
  end;
  TSRCFGSymbol = SRCFGSYMBOL;


  PSRWordA = ^TSRWordA;
  PSRWordW = ^TSRWordW;
  PSRWord = PSRWordA;
  {$EXTERNALSYM SRWORDA}
  SRWORDA = record
    dwSize:     DWORD;
    dwWordNum:  DWORD;
    szWord : record end;
    // szWord: array[0..0] of AnsiChar; //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRWORDW}
  SRWORDW = record
    dwSize:     DWORD;
    dwWordNum:  DWORD;
    szWord : record end;
    // szWord: array[0..0] of WideChar; //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRWORD}
  SRWORD = SRWORDA;
  TSRWordA = SRWORDA;
  TSRWordW = SRWORDW;
  TSRWord = TSRWordA;

  PSRPhraseA = ^TSRPhraseA;
  PSRPhraseW = ^TSRPhraseW;
  PSRPhrase = PSRPhraseA;
  {$EXTERNALSYM SRPHRASEA}
  SRPHRASEA = record
    dwSize:    DWORD;
    abWords : record end;
    //abWords: array[0..0] of BYTE;   //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRPHRASEW}
  SRPHRASEW = record
    dwSize:    DWORD;
    abWords : record end;
    //abWords: array[0..0] of BYTE;   //This is a reminder that data is appended
  end;
  {$EXTERNALSYM SRPHRASE}
  SRPHRASE = SRPHRASEA;
  TSRPhraseA = SRPHRASEA;
  TSRPhraseW = SRPHRASEW;
  TSRPhrase = TSRPhraseA;

//  PSRPHRASE = ^TSRPhrase;
//  PSRPhraseW = PSRPhrase;
//  PSRPhraseA = PSRPhrase;
//  {$EXTERNALSYM SRPHRASE}
//  SRPHRASE = record
//    dwSize:    DWORD;
//    abWords : record end;
//    //abWords: array[0..0] of BYTE;   //This is a reminder that data is appended
//  end;
//  SRPhraseW = SRPhrase;
//  SRPhraseA = SRPhrase;
//  TSRPhrase = SRPHRASE;
//  TSRPhraseW = TSRPhrase;
//  TSRPhraseA = TSRPhrase;

  PSRHeader = ^TSRHeader;
  {$EXTERNALSYM SRHEADER}
  SRHEADER = record
    dwType:  DWORD;
    dwFlags: DWORD;
  end;
  TSRHeader = SRHEADER;

  PSRChunk = ^TSRChunk;
  {$EXTERNALSYM SRCHUNK}
  SRCHUNK = record
    dwChunkID:   DWORD;
    dwChunkSize: DWORD;
    avInfo : record end;
    // avInfo: array[0..0] of BYTE;
  end;
  TSRChunk = SRCHUNK;

  PSRModeInfoA = ^TSRModeInfoA;
  PSRModeInfoW = ^TSRModeInfoW;
  PSRModeInfo = PSRModeInfoA;
  {$EXTERNALSYM SRMODEINFOA}
  SRMODEINFOA = record
    gEngineID:                  TGUID;
    szMfgName:                  array [0..SRMI_NAMELEN-1] of AnsiChar;
    szProductName:              array [0..SRMI_NAMELEN-1] of AnsiChar;
    gModeID:                    TGUID;
    szModeName:                 array [0..SRMI_NAMELEN-1] of AnsiChar;
    Language:                   TLanguageA;
    dwSequencing:               DWORD;
    dwMaxWordsVocab:            DWORD;
    dwMaxWordsState:            DWORD;
    dwGrammars:                 DWORD;
    dwFeatures:                 DWORD;
    dwInterfaces:               DWORD;
    dwEngineFeatures:           DWORD;
  end;
  {$EXTERNALSYM SRMODEINFOW}
  SRMODEINFOW = record
    gEngineID:                  TGUID;
    szMfgName:                  array [0..SRMI_NAMELEN-1] of WideChar;
    szProductName:              array [0..SRMI_NAMELEN-1] of WideChar;
    gModeID:                    TGUID;
    szModeName:                 array [0..SRMI_NAMELEN-1] of WideChar;
    Language:                   TLanguageW;
    dwSequencing:               DWORD;
    dwMaxWordsVocab:            DWORD;
    dwMaxWordsState:            DWORD;
    dwGrammars:                 DWORD;
    dwFeatures:                 DWORD;
    dwInterfaces:               DWORD;
    dwEngineFeatures:           DWORD;
  end;
  {$EXTERNALSYM SRMODEINFO}
  SRMODEINFO = SRMODEINFOA;
  TSRModeInfoA = SRMODEINFOA;
  TSRModeInfoW = SRMODEINFOW;
  TSRModeInfo = TSRModeInfoA;

  PSRModeInfoRank = ^TSRModeInfoRank;
  {$EXTERNALSYM SRMODEINFORANK}
  SRMODEINFORANK = record
    dwEngineID:          DWORD;
    dwMfgName:           DWORD;
    dwProductName:       DWORD;
    dwModeID:            DWORD;
    dwModeName:          DWORD;
    dwLanguage:          DWORD;
    dwDialect:           DWORD;
    dwSequencing:        DWORD;
    dwMaxWordsVocab:     DWORD;
    dwMaxWordsState:     DWORD;
    dwGrammars:          DWORD;
    dwFeatures:          DWORD;
    dwInterfaces:        DWORD;
    dwEngineFeatures:    DWORD;
  end;
  TSRModeInfoRank = SRMODEINFORANK;

  PSRShareA = ^TSRShareA;
  PSRShareW = ^TSRShareW;
  PSRShare = PSRShareA;
  {$EXTERNALSYM SRSHAREA}
  SRSHAREA = record
    qwInstanceID: QWORD;
    dwDeviceID:   DWORD;
    asrModeInfo:  SRMODEINFOA;
  end;
  {$EXTERNALSYM SRSHAREW}
  SRSHAREW = record
    qwInstanceID: QWORD;
    dwDeviceID:   DWORD;
    asrModeInfo:  SRMODEINFOW;
  end;
  {$EXTERNALSYM SRSHARE}
  SRSHARE = SRSHAREA;
  TSRShareA = SRSHAREA;
  TSRShareW = SRSHAREW;
  TSRShare = TSRShareA;

Const
  // Grammar Type constants
  {$EXTERNALSYM SRGRMFMT_CFG}
  SRGRMFMT_CFG                 = $0000;
  {$EXTERNALSYM SRGRMFMT_LIMITEDDOMAIN}
  SRGRMFMT_LIMITEDDOMAIN       = $0001;
  {$EXTERNALSYM SRGRMFMT_DICTATION}
  SRGRMFMT_DICTATION           = $0002;
  {$EXTERNALSYM SRGRMFMT_CFGNATIVE}
  SRGRMFMT_CFGNATIVE           = $8000;
  {$EXTERNALSYM SRGRMFMT_LIMITEDDOMAINNATIVE}
  SRGRMFMT_LIMITEDDOMAINNATIVE = $8001;
  {$EXTERNALSYM SRGRMFMT_DICTATIONNATIVE}
  SRGRMFMT_DICTATIONNATIVE     = $8002;
  {$EXTERNALSYM SRGRMFMT_DRAGONNATIVE1}
  SRGRMFMT_DRAGONNATIVE1       = $8101;
  {$EXTERNALSYM SRGRMFMT_DRAGONNATIVE2}
  SRGRMFMT_DRAGONNATIVE2       = $8102;
  {$EXTERNALSYM SRGRMFMT_DRAGONNATIVE3}
  SRGRMFMT_DRAGONNATIVE3       = $8103;
type
  {$EXTERNALSYM SRGRMFMT}
  SRGRMFMT = UINT;
  {$EXTERNALSYM PSRGRMFMT}
  PSRGRMFMT = ^SRGRMFMT;

  // SRCKD_NGRAM                                        //{SAPI 3}
  PNGramHdr = ^TNGramHdr;
  {$EXTERNALSYM NGRAMHDR}
  NGRAMHDR = record                                     //{SAPI 3}
    dwTotalCounts:         QWORD;                       //{SAPI 3}
    dwNumWordsClasses:     DWORD;                       //{SAPI 3}
    dwWordClassNameOffset: DWORD;                       //{SAPI 3}
    dwNumContextGroups:    DWORD;                       //{SAPI 3}
    dwContextGroupOffset:  DWORD;                       //{SAPI 3}
    dwNumClasses:          DWORD;                       //{SAPI 3}
    dwClassOffset:         DWORD;                       //{SAPI 3}
    bBitsPerWord:          BYTE;                        //{SAPI 3}
    abFiller:              array[0..2] of BYTE;         //{SAPI 3}
    adwProbability:        array[0..255] of DWORD;      //{SAPI 3}
  end;
  TNGramHdr = NGRAMHDR;

  PNGramCGHdr = ^TNGramCGHdr;
  {$EXTERNALSYM NGRAMCGHDR}
  NGRAMCGHDR = record                                   //{SAPI 3}
    dwSize:                DWORD;                       //{SAPI 3}
    dwTotalCounts:         QWORD;                       //{SAPI 3}
    adwWordClassID:        array[0..3] of DWORD;        //{SAPI 3}
    dwNumSequential:       DWORD;                       //{SAPI 3}
    dwNumRandomAccess:     DWORD;                       //{SAPI 3}
    bBackOffProbToken:     BYTE;                        //{SAPI 3}
    abFiller:              array[0..2] of BYTE;         //{SAPI 3}
   //{READ DOC ON THIS THERE SHOULD PROBABLY BE SOMETHING ELSE}
   // WORD or DWORD awRandomAccessWordsClasses[dwNumRandomAccess];
   // BYTE abProbToken[dwNumSequential+dwNumRandomAccess];
  end;
  TNGramCGHdr = NGRAMCGHDR;

// Low-Level Speach Recognitions 

  {$EXTERNALSYM ISRAttributesA}
  ISRAttributesA = interface(IUnknown)
    [SID_ISRAttributesA]
    function AutoGainEnableGet(var dwAutoGain: DWORD): HResult; stdcall;
    function AutoGainEnableSet(dwAutoGain: DWORD): HResult; stdcall;
    function EchoGet(var fEchoOn: BOOL): HResult; stdcall;
    function EchoSet(fEchoOn: BOOL): HResult; stdcall;
    function EnergyFloorGet(var wEnergy: WORD): HResult; stdcall;
    function EnergyFloorSet(wEnergy: WORD): HResult; stdcall;
    function MicrophoneGet(pszMicrophone: PAnsiChar; dwMicrophoneSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function MicrophoneSet(pszMicrophone: PAnsiChar): HResult; stdcall;
    function RealTimeGet(var dwRealTime: DWORD): HResult; stdcall;
    function RealTimeSet(dwRealTime: DWORD): HResult; stdcall;
    function SpeakerGet(pszSpeaker: PAnsiChar; dwSpeakerSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function SpeakerSet(pszSpeaker: PAnsiChar): HResult; stdcall;
    function TimeOutGet(var dwIncomplete: DWORD; var dwComplete: DWORD): HResult; stdcall;
    function TimeOutSet(dwIncomplete: DWORD; dwComplete: DWORD): HResult; stdcall;
    function ThresholdGet(var dwThreshold: DWORD): HResult; stdcall;
    function ThresholdSet(dwThreshold: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRATTRIBUTESA}
  PISRATTRIBUTESA = ^ISRAttributesA;
  {$EXTERNALSYM ISRAttributesW}
  ISRAttributesW = interface(IUnknown)
    [SID_ISRAttributesW]
    function AutoGainEnableGet(var dwAutoGain: DWORD): HResult; stdcall;
    function AutoGainEnableSet(dwAutoGain: DWORD): HResult; stdcall;
    function EchoGet(var fEchoOn: BOOL): HResult; stdcall;
    function EchoSet(fEchoOn: BOOL): HResult; stdcall;
    function EnergyFloorGet(var wEnergy: WORD): HResult; stdcall;
    function EnergyFloorSet(wEnergy: WORD): HResult; stdcall;
    function MicrophoneGet(pszMicrophone: PWideChar; dwMicrophoneSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function MicrophoneSet(pszMicrophone: PWideChar): HResult; stdcall;
    function RealTimeGet(var dwRealTime: DWORD): HResult; stdcall;
    function RealTimeSet(dwRealTime: DWORD): HResult; stdcall;
    function SpeakerGet(pszSpeaker: PWideChar; dwSpeakerSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function SpeakerSet(pszSpeaker: PWideChar): HResult; stdcall;
    function TimeOutGet(var dwIncomplete: DWORD; var dwComplete: DWORD): HResult; stdcall;
    function TimeOutSet(dwIncomplete: DWORD; dwComplete: DWORD): HResult; stdcall;
    function ThresholdGet(var dwThreshold: DWORD): HResult; stdcall;
    function ThresholdSet(dwThreshold: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRATTRIBUTESW}
  PISRATTRIBUTESW = ^ISRAttributesW;
  {$EXTERNALSYM ISRAttributes}
  ISRAttributes = ISRAttributesA;


  {$EXTERNALSYM ISRCentralA}
  ISRCentralA = interface(IUnknown)
    [SID_ISRCentralA]
    function ModeGet(var ModeInfo: SRMODEINFOA): HResult; stdcall;
    function GrammarLoad(eFormat: SRGRMFMT; dData: SDATA;
      pNotifyInterface: pointer; IIDNotifyInterface: TIID;
      var iUnk: IUnknown): HResult; stdcall;
    function Pause: HResult; stdcall;
    function PosnGet(var qwTimeStamp: QWORD): HResult; stdcall;
    function Resume: HResult; stdcall;
    function ToFileTime(var qWord: QWORD; var FT: TFileTime): HResult; stdcall;
    function Register(pNotifyInterface: pointer; IIDNotifyInterface: TIID;
      var dwKey: DWORD): HResult; stdcall;
    function UnRegister(dwKey: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRCENTRALA}
  PISRCENTRALA = ^ISRCentralA;
  {$EXTERNALSYM ISRCentralW}
  ISRCentralW = interface(IUnknown)
    [SID_ISRCentralW]
    function ModeGet(var ModeInfo: SRMODEINFOW): HResult; stdcall;
    function GrammarLoad(eFormat: SRGRMFMT; dData: SDATA;
      pNotifyInterface: pointer; IIDNotifyInterface: TIID;
      var iUnk: IUnknown): HResult; stdcall;
    function Pause: HResult; stdcall;
    function PosnGet(var qwTimeStamp: QWORD): HResult; stdcall;
    function Resume: HResult; stdcall;
    function ToFileTime(var qWord: QWORD; var FT: TFileTime): HResult; stdcall;
    function Register(pNotifyInterface: pointer; IIDNotifyInterface: TIID;
      var dwKey: DWORD): HResult; stdcall;
    function UnRegister(dwKey: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRCENTRALW}
  PISRCENTRALW = ^ISRCentralW;
  {$EXTERNALSYM ISRCentral}
  ISRCentral = ISRCentralA;

  {$EXTERNALSYM ISRDialogsA}
  ISRDialogsA = interface(IUnknown)
    [SID_ISRDialogsA]
    function AboutDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainMicDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainGeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRDIALOGSA}
  PISRDIALOGSA = ^ISRDialogsA;
  {$EXTERNALSYM ISRDialogsW}
  ISRDialogsW = interface(IUnknown)
    [SID_ISRDialogsW]
    function AboutDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainMicDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainGeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRDIALOGSW}
  PISRDIALOGSW = ^ISRDialogsW;
  {$EXTERNALSYM ISRDialogs}
  ISRDialogs = ISRDialogsA;

  {$EXTERNALSYM ISRDialogs2A}
  ISRDialogs2A = interface(ISRDialogsA)
    [SID_ISRDialogsA]
    function TrainPhrasesDlg(hWndParent: HWND;  pszTitle: PAnsiChar;
      pszTrainStrings: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRDIALOGS2A}
  PISRDIALOGS2A = ^ISRDialogs2A;
  {$EXTERNALSYM ISRDialogs2W}
  ISRDialogs2W = interface(ISRDialogsW)
    [SID_ISRDialogsW]
    function TrainPhrasesDlg(hWndParent: HWND;  pszTitle: PWideChar;
      pszTrainStrings: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRDIALOGS2W}
  PISRDIALOGS2W = ^ISRDialogs2W;
  {$EXTERNALSYM ISRDialogs2}
  ISRDialogs2 = ISRDialogs2A;

  {$EXTERNALSYM ISREnumA}
  ISREnumA = interface(IUnknown)
    [SID_ISREnumA]                             
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: ISREnumA): HResult; stdcall;
    function Select(gModeID: TGUID; out ISRCentral: ISRCENTRALA;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRENUMA}
  PISRENUMA = ^ISREnumA;
  {$EXTERNALSYM ISREnumW}
  ISREnumW = interface(IUnknown)
    [SID_ISREnumW]                             
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: ISREnumW): HResult; stdcall;
    function Select(gModeID: TGUID; out ISRCentral: ISRCENTRALW;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRENUMW}
  PISRENUMW = ^ISREnumW;
  {$EXTERNALSYM ISREnum}
  ISREnum = ISREnumA;

  {$EXTERNALSYM ISRFindA}
  ISRFindA = interface(IUnknown)
    [SID_ISRFindA]
    function Find(psrInfo: PSRMODEINFOA; psrInfoRank: PSRMODEINFORANK;
      var pSRInfoFound: SRMODEINFOA): HResult; stdcall;
    function Select(gModeID: TGUID; out ISRCentral: ISRCENTRALA;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRFINDA}
  PISRFINDA = ^ISRFindA;
  {$EXTERNALSYM ISRFindW}
  ISRFindW = interface(IUnknown)
    [SID_ISRFindW]
    function Find(psrInfo: PSRMODEINFOW; psrInfoRank: PSRMODEINFORANK;
      var pSRInfoFound: SRMODEINFOW): HResult; stdcall;
    function Select(gModeID: TGUID; out ISRCentral: ISRCENTRALW;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRFINDW}
  PISRFINDW = ^ISRFindW;
  {$EXTERNALSYM ISRFind}
  ISRFind = ISRFindA;

  {$EXTERNALSYM ISRGramLexPronA}
  ISRGramLexPronA = interface(IUnknown)
    [SID_ISRGramLexPronA]
    function Add(CharSet: VOICECHARSET; pszText: PAnsiChar; pszPronounce: PAnsiChar;
      PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: Pointer;
      dwEngineInfoSize: DWORD; bUnDocumented: BOOL): HResult; stdcall;
    function Get(CharSet: VOICECHARSET; pszText: PAnsiChar; wSense: WORD;
      pszPronounce: PAnsiChar; dwPronounceSize: DWORD; var dwPronounceNeeded: DWORD;
      var PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: pointer;
      dwEngineInfoSize: DWORD; var dwEngineInfoNeeded: DWORD): HResult; stdcall;
    function Remove(pszText: PAnsiChar; wSense: WORD): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PAnsiChar;
      pszUnDocumented: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMLEXPRONA}
  PISRGRAMLEXPRONA = ^ISRGramLexPronA;
  {$EXTERNALSYM ISRGramLexPronW}
  ISRGramLexPronW = interface(IUnknown)
    [SID_ISRGramLexPronW]
    function Add(CharSet: VOICECHARSET; pszText: PWideChar; pszPronounce: PWideChar;
      PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: Pointer;
      dwEngineInfoSize: DWORD; bUnDocumented: BOOL): HResult; stdcall;
    function Get(CharSet: VOICECHARSET; pszText: PWideChar; wSense: WORD;
      pszPronounce: PWideChar; dwPronounceSize: DWORD; var dwPronounceNeeded: DWORD;
      var PartOfSpeech: VOICEPARTOFSPEECH; pEngineInfo: pointer;
      dwEngineInfoSize: DWORD; var dwEngineInfoNeeded: DWORD): HResult; stdcall;
    function Remove(pszText: PWideChar; wSense: WORD): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PWideChar;
      pszUnDocumented: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMLEXPRONW}
  PISRGRAMLEXPRONW = ^ISRGramLexPronW;
  {$EXTERNALSYM ISRGramLexPron}
  ISRGramLexPron = ISRGramLexPronA;

  {$EXTERNALSYM ISRGramCommonA}
  ISRGramCommonA = interface(IUnknown)
    [SID_ISRGramCommonA]
    function Activate(hWndListening: HWND; fAutoPause: BOOL; pszInfo: PAnsiChar): HResult; stdcall;
    function Archive(fArchiveResults: BOOL; pBuf: pointer; dwBufSize: DWORD;
      var dwBufNeeded: DWORD): HResult; stdcall;
    function BookMark(qwTime: QWORD; dwBookMarkID: DWORD): HResult; stdcall;
    function Deactivate(pszInfo: PAnsiChar): HResult; stdcall;
    function DeteriorationGet(var dwMemory: DWORD; var dwTime: DWORD;
      var dwObjects: DWORD): HResult; stdcall;
    function DeteriorationSet(dwMemory: DWORD; dwTime: DWORD; dwObjects: DWORD): HResult; stdcall;
    function TrainDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainPhrase(dwExtent: DWORD; var Data: SDATA): HResult; stdcall;
    function TrainQuery(var dwFlags: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMCOMMONA}
  PISRGRAMCOMMONA = ^ISRGramCommonA;
  {$EXTERNALSYM ISRGramCommonW}
  ISRGramCommonW = interface(IUnknown)
    [SID_ISRGramCommonW]
    function Activate(hWndListening: HWND; fAutoPause: BOOL; pszInfo: PWideChar): HResult; stdcall;
    function Archive(fArchiveResults: BOOL; pBuf: pointer; dwBufSize: DWORD;
      var dwBufNeeded: DWORD): HResult; stdcall;
    function BookMark(qwTime: QWORD; dwBookMarkID: DWORD): HResult; stdcall;
    function Deactivate(pszInfo: PWideChar): HResult; stdcall;
    function DeteriorationGet(var dwMemory: DWORD; var dwTime: DWORD;
      var dwObjects: DWORD): HResult; stdcall;
    function DeteriorationSet(dwMemory: DWORD; dwTime: DWORD; dwObjects: DWORD): HResult; stdcall;
    function TrainDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainPhrase(dwExtent: DWORD; var Data: SDATA): HResult; stdcall;
    function TrainQuery(var dwFlags: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMCOMMONW}
  PISRGRAMCOMMONW = ^ISRGramCommonW;
  {$EXTERNALSYM ISRGramCommon}
  ISRGramCommon = ISRGramCommonA;

  {$EXTERNALSYM ISRGramCFGA}
  ISRGramCFGA = interface(IUnknown)
    [SID_ISRGramCFGA]
    function LinkQuery(pszLinkName: PAnsiChar; var fExist: BOOL): HResult; stdcall;
    function ListAppend(pszListName: PAnsiChar; dWord: SData): HResult; stdcall;
    function ListGet(pszListName: PAnsiChar; var dWord: SData): HResult; stdcall;
    function ListRemove(pszListName: PAnsiChar; dWord: SData): HResult; stdcall;
    function ListSet(pszListName: PAnsiChar; dWord: SData): HResult; stdcall;
    function ListQuery(pszListName: PAnsiChar; var fExist: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMCFGA}
  PISRGRAMCFGA = ^ISRGramCFGA;
  {$EXTERNALSYM ISRGramCFGW}
  ISRGramCFGW = interface(IUnknown)
    [SID_ISRGramCFGW]
    function LinkQuery(pszLinkName: PWideChar; var fExist: BOOL): HResult; stdcall;
    function ListAppend(pszListName: PWideChar; dWord: SData): HResult; stdcall;
    function ListGet(pszListName: PWideChar; var dWord: SData): HResult; stdcall;
    function ListRemove(pszListName: PWideChar; dWord: SData): HResult; stdcall;
    function ListSet(pszListName: PWideChar; dWord: SData): HResult; stdcall;
    function ListQuery(pszListName: PWideChar; var fExist: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMCFGW}
  PISRGRAMCFGW = ^ISRGramCFGW;
  {$EXTERNALSYM ISRGramCFG}
  ISRGramCFG = ISRGramCFGA;

  {$EXTERNALSYM ISRGramDictationA}
  ISRGramDictationA = interface(IUnknown)
    [SID_ISRGramDictationA]
    function Context(pszPrior: PAnsiChar; pszAfter: PAnsiChar): HResult; stdcall;
    function Hint(pszHint: PAnsiChar): HResult; stdcall;
    function Words(pszWords: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMDICTATIONA}
  PISRGRAMDICTATIONA = ^ISRGramDictationA;
  {$EXTERNALSYM ISRGramDictationW}
  ISRGramDictationW = interface(IUnknown)
    [SID_ISRGramDictationW]
    function Context(pszPrior: PWideChar; pszAfter: PWideChar): HResult; stdcall;
    function Hint(pszHint: PWideChar): HResult; stdcall;
    function Words(pszWords: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMDICTATIONW}
  PISRGRAMDICTATIONW = ^ISRGramDictationW;
  {$EXTERNALSYM ISRGramDictation}
  ISRGramDictation = ISRGramDictationA;

  {$EXTERNALSYM ISRGramInsertionGUI}
  ISRGramInsertionGUI = interface(IUnknown)
    [SID_ISRGramInsertionGUI]
    function Hide: HResult; stdcall;
    function Move(rCursor: TRect): HResult; stdcall;
    function Show(hWnd: HWND): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRGRAMINSERTIONGUI}
  PISRGRAMINSERTIONGUI = ^ISRGramInsertionGUI;

  {$EXTERNALSYM ISRGramNotifySinkA}
  ISRGramNotifySinkA = interface(IUnknown)
    [SID_ISRGramNotifySinkA]
    function BookMark(dwBookMarkID: DWORD): HResult; stdcall;
    function Paused: HResult; stdcall;
    function PhraseFinish(dwFlags: DWORD; qTimeStampBegin: QWORD;
      qTimeStampEnd: QWORD; pSRPhrase: PSRPhraseA; IUnknownResult: IUnknown): HResult; stdcall;
    function PhraseHypothesis(dwFlags: DWORD; qTimeStampBegin: QWORD;
      qTimeStampEnd: QWORD; pSRPhrase: PSRPhraseA; IUnknownResult: IUnknown): HResult; stdcall;
    function PhraseStart(qTimeStampBegin: QWORD): HResult; stdcall;
    function ReEvaluate(IUnk: IUnknown): HResult; stdcall;
    function Training(dwTrain: DWORD): HResult; stdcall;
    function UnArchive(IUnknownResult: IUnknown): HResult; stdcall;
  end; //ISRGramNotifySink
  {$EXTERNALSYM PISRGRAMNOTIFYSINKA}
  PISRGRAMNOTIFYSINKA = ^ISRGramNotifySinkA;
  {$EXTERNALSYM ISRGramNotifySinkW}
  ISRGramNotifySinkW = interface(IUnknown)
    [SID_ISRGramNotifySinkW]
    function BookMark(dwBookMarkID: DWORD): HResult; stdcall;
    function Paused: HResult; stdcall;
    function PhraseFinish(dwFlags: DWORD; qTimeStampBegin: QWORD;
      qTimeStampEnd: QWORD; pSRPhrase: PSRPhraseW; IUnknownResult: IUnknown): HResult; stdcall;
    function PhraseHypothesis(dwFlags: DWORD; qTimeStampBegin: QWORD;
      qTimeStampEnd: QWORD; pSRPhrase: PSRPhraseW; IUnknownResult: IUnknown): HResult; stdcall;
    function PhraseStart(qTimeStampBegin: QWORD): HResult; stdcall;
    function ReEvaluate(IUnk: IUnknown): HResult; stdcall;
    function Training(dwTrain: DWORD): HResult; stdcall;
    function UnArchive(IUnknownResult: IUnknown): HResult; stdcall;
  end; //ISRGramNotifySink
  {$EXTERNALSYM PISRGRAMNOTIFYSINKW}
  PISRGRAMNOTIFYSINKW = ^ISRGramNotifySinkW;
  {$EXTERNALSYM ISRGramNotifySink}
  ISRGramNotifySink = ISRGramNotifySinkA;

  {$EXTERNALSYM ISRNotifySink}
  ISRNotifySink = interface(IUnknown)
    [SID_ISRNotifySink]
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Interference(qTimeStampBegin: QWORD; qTimeStampEnd: QWORD;
      dwType: DWORD): HResult; stdcall;
    function Sound(qTimeStampBegin: QWORD; qTimeStampEnd: QWORD): HResult; stdcall;
    function UtteranceBegin(qTimeStampBegin: QWORD): HResult; stdcall;
    function UtteranceEnd(qTimeStampBegin: QWORD; qTimeStampEnd: QWORD): HResult; stdcall;
    function VUMeter(qTimeStampBegin: QWORD; wLevel: WORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRNOTIFYSINK}
  PISRNOTIFYSINK = ^ISRNotifySink;

  {$EXTERNALSYM ISRNotifySink2}
  ISRNotifySink2 = interface(ISRNotifySink)
    [SID_ISRNotifySink2]
    function Training: HResult; stdcall;
    function Error(pError: IUnknown): HResult; stdcall;
    function Warning(pWarning: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRNOTIFYSINK2}
  PISRNOTIFYSINK2 = ^ISRNotifySink2;

  {$EXTERNALSYM ISRResBasicA}
  ISRResBasicA = interface(IUnknown)
    [SID_ISRResBasicA]
    function PhraseGet(dwRank: DWORD; pSRPhrase: PSRPHRASEA;
      dwPhraseSize: DWORD; var dwPhraseNeeded: DWORD): HResult; stdcall;
    function Identify(var gIdentity: TGUID): HResult; stdcall;
    function TimeGet(var qTimeStampBegin: QWORD; var qTimeStampEnd: QWORD): HResult; stdcall;
    function FlagsGet(dwRank: DWORD; var dwFlags: DWORD): HResult; stdcall;
  end; //ISRResBasic
  {$EXTERNALSYM PISRRESBASICA}
  PISRRESBASICA = ^ISRResBasicA;
  {$EXTERNALSYM ISRResBasicW}
  ISRResBasicW = interface(IUnknown)
    [SID_ISRResBasicW]
    function PhraseGet(dwRank: DWORD; pSRPhrase: PSRPHRASEW;
      dwPhraseSize: DWORD; var dwPhraseNeeded: DWORD): HResult; stdcall;
    function Identify(var gIdentity: TGUID): HResult; stdcall;
    function TimeGet(var qTimeStampBegin: QWORD; var qTimeStampEnd: QWORD): HResult; stdcall;
    function FlagsGet(dwRank: DWORD; var dwFlags: DWORD): HResult; stdcall;
  end; //ISRResBasic
  {$EXTERNALSYM PISRRESBASICW}
  PISRRESBASICW = ^ISRResBasicW;
  {$EXTERNALSYM ISRResBasic}
  ISRResBasic = ISRResBasicA;

  {$EXTERNALSYM ISRResScores}
  ISRResScores = interface(IUnknown)
    [SID_ISRResScores]
    function GetPhraseScore(dwRank: DWORD; var lScore: LongInt): HResult; stdcall;
    function GetWordScores(dwRank: DWORD; var alScore: LongInt;
      dwWordScoreSize: DWORD; var dwWordScoreRequired: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESSCORES}
  PISRRESSCORES = ^ISRResScores;

  {$EXTERNALSYM ISRResMerge}
  ISRResMerge = interface(IUnknown)
    [SID_ISRResMerge]
    function Merge(IUnkAdjacent: IUnknown; var IUnkMerge: IUnknown): HResult; stdcall;
    function Split(qwSplitTime: QWORD; var IUnkLeft: IUnknown;
    var IUnkRight: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESMERGE}
  PISRRESMERGE = ^ISRResMerge;

  {$EXTERNALSYM ISRResAudio}
  ISRResAudio = interface(IUnknown)
    [SID_ISRResAudio]
    function GetWAV(var Wav: SDATA): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESAUDIO}
  PISRRESAUDIO = ^ISRResAudio;

  {$EXTERNALSYM ISRResAudioEx}
  ISRResAudioEx = interface(IUnknown)               //SAPI 4
    [SID_ISRResAudioEx]
    function GetWAV(var Data: SDATA; qwStart: QWORD; qwEnd: QWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESAUDIOEX}
  PISRRESAUDIOEX = ^ISRResAudioEx;


  {$EXTERNALSYM ISRResCorrectionA}
  ISRResCorrectionA = interface(IUnknown)
    [SID_ISRResCorrectionA]
    function Correction(pSRPhrase: PSRPHRASEA; wConfidence: WORD): HResult; stdcall;
    function Validate(wConfidence: WORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESCORRECTIONA}
  PISRRESCORRECTIONA = ^ISRResCorrectionA;
  {$EXTERNALSYM ISRResCorrectionW}
  ISRResCorrectionW = interface(IUnknown)
    [SID_ISRResCorrectionW]
    function Correction(pSRPhrase: PSRPHRASEW; wConfidence: WORD): HResult; stdcall;
    function Validate(wConfidence: WORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESCORRECTIONW}
  PISRRESCORRECTIONW = ^ISRResCorrectionW;
  {$EXTERNALSYM ISRResCorrection}
  ISRResCorrection = ISRResCorrectionA;

  {$EXTERNALSYM ISRResEval}
  ISRResEval = interface(IUnknown)
    [SID_ISRResEval]
    function ReEvaluate(var fChanged: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESEVAL}
  PISRRESEVAL = ^ISRResEval;

  {$EXTERNALSYM ISRResGraphA}
  ISRResGraphA = interface(IUnknown)
    [SID_ISRResGraphA]
    function BestPathPhoneme(dwRank: DWORD; padwPath: Pointer; dwPathSize: DWORD;
      var dwPathNeeded: DWORD): HResult; stdcall;
    function BestPathWord(dwRank: DWORD; padwPath: Pointer; dwPathSize: DWORD;
      var dwPathNeeded: DWORD): HResult; stdcall;
    function GetPhonemeNode(dwPhonemeNode: DWORD; pNode: PSRRESPHONEMENODE;
      pcIPA: PWideChar; pcEngine: PWideChar): HResult; stdcall;  // check pcEngine param
    function GetWordNode(dwWordNode: DWORD; pNode: PSRRESWORDNODE;
      apSRWord: PSRWORDA; dwMemSize: DWORD; var dwMemNeeded: DWORD): HResult; stdcall;
    function PathScorePhoneme(var paNodes: pointer; dwNumNodes: DWORD;
      var lScore: LongInt): HResult; stdcall;
    function PathScoreWord(var paNodes: pointer; dwNumNodes: DWORD;
      var lScore: LongInt): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESGRAPHA}
  PISRRESGRAPHA = ^ISRResGraphA;
  {$EXTERNALSYM ISRResGraphW}
  ISRResGraphW = interface(IUnknown)
    [SID_ISRResGraphW]
    function BestPathPhoneme(dwRank: DWORD; padwPath: Pointer; dwPathSize: DWORD;
      var dwPathNeeded: DWORD): HResult; stdcall;
    function BestPathWord(dwRank: DWORD; padwPath: Pointer; dwPathSize: DWORD;
      var dwPathNeeded: DWORD): HResult; stdcall;
    function GetPhonemeNode(dwPhonemeNode: DWORD; pNode: PSRRESPHONEMENODE;
      pcIPA: PWideChar; pcEngine: PWideChar): HResult; stdcall;  // check pcEngine param
    function GetWordNode(dwWordNode: DWORD; pNode: PSRRESWORDNODE;
      apSRWord: PSRWORDW; dwMemSize: DWORD; var dwMemNeeded: DWORD): HResult; stdcall;
    function PathScorePhoneme(var paNodes: pointer; dwNumNodes: DWORD;
      var lScore: LongInt): HResult; stdcall;
    function PathScoreWord(var paNodes: pointer; dwNumNodes: DWORD;
      var lScore: LongInt): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESGRAPHW}
  PISRRESGRAPHW = ^ISRResGraphW;
  {$EXTERNALSYM ISRResGraph}
  ISRResGraph = ISRResGraphA;


  {$EXTERNALSYM ISRResGraphEx}
  ISRResGraphEx = interface(IUnknown)
    function NodeStartGet(var dwNodeID: DWORD): HResult; stdcall;
    function NodeEndGet(var dwNodeID: DWORD): HResult; stdcall;
    function ArcEnum(dwNodeID: DWORD; var dwBuf: DWORD; dwSize: DWORD;
      var dwNum: DWORD; fOutgoing: BOOL): HResult; stdcall;
    function NodeGet(dwArcID: DWORD; var dwNode: DWORD; fDestination: BOOL): HResult; stdcall;
    function DWORDGet(dwID: DWORD; gAttrib: TGUID; var dwValue: DWORD): HResult; stdcall;
    function DataGet(dwID: DWORD; gAttrib: TGUID; var sData: SDATA): HResult; stdcall;
    function ScoreGet(dwScoreType: DWORD; var adwPath: DWORD;
      dwPathSteps: DWORD; dwPathIndexStart: DWORD; dwPathIndexCount: DWORD;
      var dwScore: DWORD): HResult; stdcall;
    function BestPathEnum(dwRank: DWORD; var adwStartPath: DWORD;
      dwStartPathSteps: DWORD; var adwEndPath: DWORD; dwEndPathSteps: DWORD;
      fExactMatch: BOOL; var sData: SDATA): HResult; stdcall;
    function GetAllArcs(var adwArcID: DWORD; dwSize: DWORD;
      var dwNumArcs: DWORD; var dwNeeded: DWORD): HResult; stdcall;
    function GetAllNodes(var adwNodeID: DWORD; dwSize: DWORD;
      var dwNumNodes: DWORD; var dwNeeded: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESGRAPHEX}
  PISRRESGRAPHEX = ^ISRResGraphEx;

  {$EXTERNALSYM ISRResMemory}
  ISRResMemory = interface(IUnknown)
    [SID_ISRResMemory]
    function Free(dwKind: DWORD): HResult; stdcall;
    function Get(var dwKind: DWORD; var dwMemory: DWORD): HResult; stdcall;
    function LockGet(var fLock: BOOL): HResult; stdcall;
    function LockSet(fLock: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESMEMORY}
  PISRRESMEMORY = ^ISRResMemory;

  {$EXTERNALSYM ISRResModifyGUI}
  ISRResModifyGUI = interface(IUnknown)
    [SID_ISRResModifyGUI]
    function Hide: HResult; stdcall;
    function Move(var rSelection: TRect): HResult; stdcall;
    function Show(hWnd: HWND): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESMODIFYGUI}
  PISRRESMODIFYGUI = ^ISRResModifyGUI;

  {$EXTERNALSYM ISRResSpeakerA}
  ISRResSpeakerA = interface(IUnknown)
    [SID_ISRResSpeakerA]
    function Correction(pszName: PAnsiChar; wConfidence: WORD): HResult; stdcall;
    function Validate(wConfidence: WORD): HResult; stdcall;
    function Identify(dwRank: DWORD; pszName: PAnsiChar; dwNameSize: DWORD;
      var dwNeeded: DWORD; var lScore: LongInt): HResult; stdcall;
    function IdentifyForFree(var fFree: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESSPEAKERA}
  PISRRESSPEAKERA = ^ISRResSpeakerA;
  {$EXTERNALSYM ISRResSpeakerW}
  ISRResSpeakerW = interface(IUnknown)
    [SID_ISRResSpeakerW]
    function Correction(pszName: PWideChar; wConfidence: WORD): HResult; stdcall;
    function Validate(wConfidence: WORD): HResult; stdcall;
    function Identify(dwRank: DWORD; pszName: PWideChar; dwNameSize: DWORD;
      var dwNeeded: DWORD; var lScore: LongInt): HResult; stdcall;
    function IdentifyForFree(var fFree: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRRESSPEAKERW}
  PISRRESSPEAKERW = ^ISRResSpeakerW;
  {$EXTERNALSYM ISRResSpeaker}
  ISRResSpeaker = ISRResSpeakerA;

  {$EXTERNALSYM ISRSpeakerA}
  ISRSpeakerA = interface(IUnknown)
    [SID_ISRSpeakerA]
    function Delete(pszSpeakerName: PAnsiChar): HResult; stdcall;
    function Enum(var pszBuffer: PAnsiChar; var dwBufSize: DWORD): HResult; stdcall;
    function Merge(pszSpeakerName: PAnsiChar; pSpeakerData: Pointer;
      dwSpeakerData: DWORD): HResult; stdcall;
    function New(pszSpeakerName: PAnsiChar): HResult; stdcall;
    function Query(pszSpeakerName: PAnsiChar; dwSize: DWORD; var dwNeeded: DWORD): HResult; stdcall;
    function Read(pszSpeakerName: PAnsiChar; var pBuffer: Pointer;
      var dwBufSize: DWORD): HResult; stdcall;
    function Revert(pszSpeakerName: PAnsiChar): HResult; stdcall;
    function Select(pszSpeakerName: PAnsiChar; fLock: BOOL): HResult; stdcall;
    function Write(pszSpeakerName: PAnsiChar; pSpeakerData: Pointer;
      dwSpeakerData: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRSPEAKERA}
  PISRSPEAKERA = ^ISRSpeakerA;
  {$EXTERNALSYM ISRSpeakerW}
  ISRSpeakerW = interface(IUnknown)
    [SID_ISRSpeakerW]
    function Delete(pszSpeakerName: PWideChar): HResult; stdcall;
    function Enum(var pszBuffer: PWideChar; var dwBufSize: DWORD): HResult; stdcall;
    function Merge(pszSpeakerName: PWideChar; pSpeakerData: Pointer;
      dwSpeakerData: DWORD): HResult; stdcall;
    function New(pszSpeakerName: PWideChar): HResult; stdcall;
    function Query(pszSpeakerName: PWideChar; dwSize: DWORD; var dwNeeded: DWORD): HResult; stdcall;
    function Read(pszSpeakerName: PWideChar; var pBuffer: Pointer;
      var dwBufSize: DWORD): HResult; stdcall;
    function Revert(pszSpeakerName: PWideChar): HResult; stdcall;
    function Select(pszSpeakerName: PWideChar; fLock: BOOL): HResult; stdcall;
    function Write(pszSpeakerName: PWideChar; pSpeakerData: Pointer;
      dwSpeakerData: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRSPEAKERW}
  PISRSPEAKERW = ^ISRSpeakerW;
  {$EXTERNALSYM ISRSpeaker}
  ISRSpeaker = ISRSpeakerA;

  {$EXTERNALSYM ISRSpeaker2A}
  ISRSpeaker2A = interface(ISRSpeakerA)
    [SID_ISRSpeaker2A]
    function Commit: HResult; stdcall;
    function Rename(pszOldName: PAnsiChar; pszNewName: PAnsiChar): HResult; stdcall;
    function GetChangedInfo(var fChanged: BOOL; var ft: TFileTime): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRSPEAKER2A}
  PISRSPEAKER2A = ^ISRSpeaker2A;
  {$EXTERNALSYM ISRSpeaker2W}
  ISRSpeaker2W = interface(ISRSpeakerW)
    [SID_ISRSpeaker2W]
    function Commit: HResult; stdcall;
    function Rename(pszOldName: PWideChar; pszNewName: PWideChar): HResult; stdcall;
    function GetChangedInfo(var fChanged: BOOL; var ft: TFileTime): HResult; stdcall;
  end;
  {$EXTERNALSYM PISRSPEAKER2W}
  PISRSPEAKER2W = ^ISRSpeaker2W;
  {$EXTERNALSYM ISRSpeaker2}
  ISRSpeaker2 = ISRSpeaker2A;


//************************************************************************
// Low-Level text-to-speech API
///************************************************************************
const
  {$EXTERNALSYM TTSI_NAMELEN}
  TTSI_NAMELEN                 = SVFN_LEN;
  {$EXTERNALSYM TTSI_STYLELEN}
  TTSI_STYLELEN                = SVFN_LEN;

  {$EXTERNALSYM GENDER_NEUTRAL}
  GENDER_NEUTRAL               =  0;
  {$EXTERNALSYM GENDER_FEMALE}
  GENDER_FEMALE                =  1;
  {$EXTERNALSYM GENDER_MALE}
  GENDER_MALE                  =  2;

  {$EXTERNALSYM TTSFEATURE_ANYWORD}
  TTSFEATURE_ANYWORD          = $00000001 ;     //{bit 0}
  {$EXTERNALSYM TTSFEATURE_VOLUME}
  TTSFEATURE_VOLUME           = $00000002 ;     //{bit 1}
  {$EXTERNALSYM TTSFEATURE_SPEED}
  TTSFEATURE_SPEED            = $00000004 ;     //{bit 2}
  {$EXTERNALSYM TTSFEATURE_PITCH}
  TTSFEATURE_PITCH            = $00000008 ;     //{bit 3}
  {$EXTERNALSYM TTSFEATURE_TAGGED}
  TTSFEATURE_TAGGED           = $00000010 ;     //{bit 4}
  {$EXTERNALSYM TTSFEATURE_IPAUNICODE}
  TTSFEATURE_IPAUNICODE       = $00000020 ;     //{bit 5}
  {$EXTERNALSYM TTSFEATURE_VISUAL}
  TTSFEATURE_VISUAL           = $00000040 ;     //{bit 6}
  {$EXTERNALSYM TTSFEATURE_WORDPOSITION}
  TTSFEATURE_WORDPOSITION     = $00000080 ;     //{bit 7}
  {$EXTERNALSYM TTSFEATURE_PCOPTIMIZED}
  TTSFEATURE_PCOPTIMIZED      = $00000100 ;     //{bit 8}
  {$EXTERNALSYM TTSFEATURE_PHONEOPTIMIZED}
  TTSFEATURE_PHONEOPTIMIZED   = $00000200 ;     //{bit 9}
  {$EXTERNALSYM TTSFEATURE_FIXEDAUDIO}
  TTSFEATURE_FIXEDAUDIO       = $00000400 ;     //{bit 10}   SAPI 4
  {$EXTERNALSYM TTSFEATURE_SINGLEINSTANCE}
  TTSFEATURE_SINGLEINSTANCE   = $00000800 ;     //{bit 11}   SAPI 4
  {$EXTERNALSYM TTSFEATURE_THREADSAFE}
  TTSFEATURE_THREADSAFE       = $00001000 ;     //{bit 12}   SAPI 4
  {$EXTERNALSYM TTSFEATURE_IPATEXTDATA}
  TTSFEATURE_IPATEXTDATA      = $00002000 ;     //{bit 13}   SAPI 4
  {$EXTERNALSYM TTSFEATURE_PREFERRED}
  TTSFEATURE_PREFERRED        = $00004000 ;     //{bit 14}   SAPI 4
  {$EXTERNALSYM TTSFEATURE_TRANSPLANTED}
  TTSFEATURE_TRANSPLANTED     = $00008000 ;     //{bit 15}   SAPI 4
  {$EXTERNALSYM TTSFEATURE_SAPI4}
  TTSFEATURE_SAPI4            = $00010000 ;     //{bit 16}   SAPI 4

  {$EXTERNALSYM TTSI_ILEXPRONOUNCE}
  TTSI_ILEXPRONOUNCE          = $00000001 ;     //{bit 0}
  {$EXTERNALSYM TTSI_ITTSATTRIBUTES}
  TTSI_ITTSATTRIBUTES         = $00000002 ;     //{bit 1}
  {$EXTERNALSYM TTSI_ITTSCENTRAL}
  TTSI_ITTSCENTRAL            = $00000004 ;     //{bit 2}
  {$EXTERNALSYM TTSI_ITTSDIALOGS}
  TTSI_ITTSDIALOGS            = $00000008 ;     //{bit 3}
  {$EXTERNALSYM TTSI_ATTRIBUTES}
  TTSI_ATTRIBUTES             = $00000010 ;     //{bit 4}   SAPI 4
  {$EXTERNALSYM TTSI_IATTRIBUTES}
  TTSI_IATTRIBUTES            = $00000010 ;     //{bit 4}   SAPI 4
  {$EXTERNALSYM TTSI_ILEXPRONOUNCE2}
  TTSI_ILEXPRONOUNCE2         = $00000020 ;     //{bit 5}   SAPI 4

  {$EXTERNALSYM TTSDATAFLAG_TAGGED}
  TTSDATAFLAG_TAGGED          = $00000001 ;     //{bit 0}
  {$EXTERNALSYM TTSBNS_ABORTED}
  TTSBNS_ABORTED              = $00000001 ;     //{bit 0}

  {$EXTERNALSYM TTSNSAC_REALTIME}
  TTSNSAC_REALTIME           =   0;
  {$EXTERNALSYM TTSNSAC_PITCH}
  TTSNSAC_PITCH              =   1;
  {$EXTERNALSYM TTSNSAC_SPEED}
  TTSNSAC_SPEED              =   2;
  {$EXTERNALSYM TTSNSAC_VOLUME}
  TTSNSAC_VOLUME             =   3;

  {$EXTERNALSYM TTSNSHINT_QUESTION}
  TTSNSHINT_QUESTION         = $00000001 ;     //{bit 0}
  {$EXTERNALSYM TTSNSHINT_STATEMENT}
  TTSNSHINT_STATEMENT        = $00000002 ;     //{bit 1}
  {$EXTERNALSYM TTSNSHINT_COMMAND}
  TTSNSHINT_COMMAND          = $00000004 ;     //{bit 2}
  {$EXTERNALSYM TTSNSHINT_EXCLAMATION}
  TTSNSHINT_EXCLAMATION      = $00000008 ;     //{bit 3}
  {$EXTERNALSYM TTSNSHINT_EMPHASIS}
  TTSNSHINT_EMPHASIS         = $00000010 ;     //{bit 4}

  {$EXTERNALSYM TTSAGE_BABY}
  TTSAGE_BABY                 = 1;
  {$EXTERNALSYM TTSAGE_TODDLER}
  TTSAGE_TODDLER              = 3;
  {$EXTERNALSYM TTSAGE_CHILD}
  TTSAGE_CHILD                = 6;
  {$EXTERNALSYM TTSAGE_ADOLESCENT}
  TTSAGE_ADOLESCENT           = 14;
  {$EXTERNALSYM TTSAGE_ADULT}
  TTSAGE_ADULT                = 30;
  {$EXTERNALSYM TTSAGE_ELDERLY}
  TTSAGE_ELDERLY              = 70;

  {$EXTERNALSYM TTSATTR_MINPITCH}
  TTSATTR_MINPITCH            = 0;
  {$EXTERNALSYM TTSATTR_MAXPITCH}
  TTSATTR_MAXPITCH            = $ffff;
  {$EXTERNALSYM TTSATTR_MINREALTIME}
  TTSATTR_MINREALTIME         = 0;
  {$EXTERNALSYM TTSATTR_MAXREALTIME}
  TTSATTR_MAXREALTIME         = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MINSPEED}
  TTSATTR_MINSPEED            = 0;
  {$EXTERNALSYM TTSATTR_MAXSPEED}
  TTSATTR_MAXSPEED            = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MINVOLUME}
  TTSATTR_MINVOLUME           = 0;
  {$EXTERNALSYM TTSATTR_MAXVOLUME}
  TTSATTR_MAXVOLUME           = DWORD($ffffffff);

type
  PTTSMouth = ^TTTSMouth;
  {$EXTERNALSYM TTSMOUTH}
  TTSMOUTH = record
    bMouthHeight:        BYTE;
    bMouthWidth:         BYTE;
    bMouthUpturn:        BYTE;
    bJawOpen:            BYTE;
    bTeethUpperVisible:  BYTE;
    bTeethLowerVisible:  BYTE;
    bTonguePosn:         BYTE;
    bLipTension:         BYTE;
  end;
  TTTSMouth = TTSMOUTH;

  PTTSModeInfoA = ^TTTSModeInfoA;
  PTTSModeInfoW = ^TTTSModeInfoW;
  PTTSModeInfo = PTTSModeInfoA;
  {$EXTERNALSYM TTSMODEINFOA}
  TTSMODEINFOA = record
    gEngineID        : TGUID;
    szMfgName        : array [0..TTSI_NAMELEN-1] of AnsiChar;
    szProductName    : array [0..TTSI_NAMELEN-1] of AnsiChar;
    gModeID          : TGUID;
    szModeName       : array [0..TTSI_NAMELEN-1] of AnsiChar;
    Language         : TLanguageA;
    szSpeaker        : array [0..TTSI_NAMELEN-1] of AnsiChar;
    szStyle          : array [0..TTSI_STYLELEN-1] of AnsiChar;
    wGender          : WORD;
    wAge             : WORD;
    dwFeatures       : DWORD;
    dwInterfaces     : DWORD;
    dwEngineFeatures : DWORD;
  end;
  {$EXTERNALSYM TTSMODEINFOW}
  TTSMODEINFOW = record
    gEngineID        : TGUID;
    szMfgName        : array [0..TTSI_NAMELEN-1] of WideChar;
    szProductName    : array [0..TTSI_NAMELEN-1] of WideChar;
    gModeID          : TGUID;
    szModeName       : array [0..TTSI_NAMELEN-1] of WideChar;
    Language         : TLanguageW;
    szSpeaker        : array [0..TTSI_NAMELEN-1] of WideChar;
    szStyle          : array [0..TTSI_STYLELEN-1] of WideChar;
    wGender          : WORD;
    wAge             : WORD;
    dwFeatures       : DWORD;
    dwInterfaces     : DWORD;
    dwEngineFeatures : DWORD;
  end;
  {$EXTERNALSYM TTSMODEINFO}
  TTSMODEINFO = TTSMODEINFOA;
  TTTSModeInfoA = TTSMODEINFOA;
  TTTSModeInfoW = TTSMODEINFOW;
  TTTSModeInfo = TTTSModeInfoA;

  PTTSModeInfoRank = ^TTTSModeInfoRank;
  {$EXTERNALSYM TTSMODEINFORANK}
  TTSMODEINFORANK = record
    dwEngineID       : DWORD;
    dwMfgName        : DWORD;
    dwProductName    : DWORD;
    dwModeID         : DWORD;
    dwModeName       : DWORD;
    dwLanguage       : DWORD;
    dwDialect        : DWORD;
    dwSpeaker        : DWORD;
    dwStyle          : DWORD;
    dwGender         : DWORD;
    dwAge            : DWORD;
    dwFeatures       : DWORD;
    dwInterfaces     : DWORD;
    dwEngineFeatures : DWORD;
  end;
  TTTSModeInfoRank = TTSMODEINFORANK;

  {$EXTERNALSYM ITTSAttributesA}
  ITTSAttributesA = interface(IUnknown)
    [SID_ITTSAttributesA]
    function PitchGet(var wPitch: WORD): HResult; stdcall;
    function PitchSet(wPitch: WORD): HResult; stdcall;
    function RealTimeGet(var dwRealTime: DWORD): HResult; stdcall;
    function RealTimeSet(dwRealTime: DWORD): HResult; stdcall;
    function SpeedGet(var dwSpeed: DWORD): HResult; stdcall;
    function SpeedSet(dwSpeed: DWORD): HResult; stdcall;
    function VolumeGet(var dwVolume: DWORD): HResult; stdcall;
    function VolumeSet(dwVolume: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSATTRIBUTESA}
  PITTSATTRIBUTESA = ^ITTSAttributesA;
  {$EXTERNALSYM ITTSAttributesW}
  ITTSAttributesW = interface(IUnknown)
    [SID_ITTSAttributesW]
    function PitchGet(var wPitch: WORD): HResult; stdcall;
    function PitchSet(wPitch: WORD): HResult; stdcall;
    function RealTimeGet(var dwRealTime: DWORD): HResult; stdcall;
    function RealTimeSet(dwRealTime: DWORD): HResult; stdcall;
    function SpeedGet(var dwSpeed: DWORD): HResult; stdcall;
    function SpeedSet(dwSpeed: DWORD): HResult; stdcall;
    function VolumeGet(var dwVolume: DWORD): HResult; stdcall;
    function VolumeSet(dwVolume: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSATTRIBUTESW}
  PITTSATTRIBUTESW = ^ITTSAttributesW;
  {$EXTERNALSYM ITTSAttributes}
  ITTSAttributes = ITTSAttributesA;
  {$EXTERNALSYM PITTSATTRIBUTES}
  PITTSATTRIBUTES = ^ITTSAttributes;

  {$EXTERNALSYM ITTSBufNotifySink}
  ITTSBufNotifySink = interface(IUnknown)
    [SID_ITTSBufNotifySink]
    function TextDataDone(qTimeStamp: QWORD; dwFlags: DWORD): HResult; stdcall;
    function TextDataStarted(qTimeStamp: QWORD): HResult; stdcall;
    function BookMark(qTimeStamp: QWORD; dwMarkNum: DWORD): HResult; stdcall;
    function WordPosition(qTimeStamp: QWORD; dwByteOffset: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSBUFNOTIFYSINK}
  PITTSBUFNOTIFYSINK = ^ITTSBufNotifySink;

  {$EXTERNALSYM ITTSCentralA}
  ITTSCentralA = interface(IUnknown)
    [SID_ITTSCentralA]
    function Inject(pszTag: PAnsiChar): HResult; stdcall;
    function ModeGet(var ttsInfo: TTTSModeInfoA): HResult; stdcall;
    function Phoneme(eCharacterSet: VOICECHARSET; dwFlags: DWORD; dText: SDATA;
      var dPhoneme: SDATA): HResult; stdcall;
    function PosnGet(var qwTimeStamp: QWORD): HResult; stdcall;
    function TextData(eCharacterSet: VOICECHARSET; dwFlags: DWORD; dText: SDATA;
      pNotifyInterface: Pointer; IIDNotifyInterface: TGUID): HResult; stdcall;
    function ToFileTime(var qTimeStamp: QWORD; var FT: TFileTime): HResult; stdcall;
    function AudioPause: HResult; stdcall;
    function AudioResume: HResult; stdcall;
    function AudioReset: HResult; stdcall;
    function Register(pNotifyInterface: Pointer; IIDNotifyInterface: TGUID;
      var dwKey: DWORD): HResult; stdcall;
    function UnRegister(dwKey: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSCENTRALA}
  PITTSCENTRALA = ^ITTSCentralA;
  {$EXTERNALSYM ITTSCentralW}
  ITTSCentralW = interface(IUnknown)
    [SID_ITTSCentralW]
    function Inject(pszTag: PWideChar): HResult; stdcall;
    function ModeGet(var ttsInfo: TTTSModeInfoW): HResult; stdcall;
    function Phoneme(eCharacterSet: VOICECHARSET; dwFlags: DWORD; dText: SDATA;
      var dPhoneme: SDATA): HResult; stdcall;
    function PosnGet(var qwTimeStamp: QWORD): HResult; stdcall;
    function TextData(eCharacterSet: VOICECHARSET; dwFlags: DWORD; dText: SDATA;
      pNotifyInterface: Pointer; IIDNotifyInterface: TGUID): HResult; stdcall;
    function ToFileTime(var qTimeStamp: QWORD; var FT: TFileTime): HResult; stdcall;
    function AudioPause: HResult; stdcall;
    function AudioResume: HResult; stdcall;
    function AudioReset: HResult; stdcall;
    function Register(pNotifyInterface: Pointer; IIDNotifyInterface: TGUID;
      var dwKey: DWORD): HResult; stdcall;
    function UnRegister(dwKey: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSCENTRALW}
  PITTSCENTRALW = ^ITTSCentralW;
  {$EXTERNALSYM ITTSCentral}
  ITTSCentral = ITTSCentralA;

  {$EXTERNALSYM ITTSDialogsA}
  ITTSDialogsA = interface(IUnknown)
    [SID_ITTSDialogsA]
    function AboutDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TranslateDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSDIALOGSA}
  PITTSDIALOGSA = ^ITTSDialogsA;
  {$EXTERNALSYM ITTSDialogsW}
  ITTSDialogsW = interface(IUnknown)
    [SID_ITTSDialogsW]
    function AboutDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TranslateDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSDIALOGSW}
  PITTSDIALOGSW = ^ITTSDialogsW;
  {$EXTERNALSYM ITTSDialogs}
  ITTSDialogs = ITTSDialogsA;

  {$EXTERNALSYM ITTSEnumA}
  ITTSEnumA = interface(IUnknown)
    [SID_ITTSEnumA]
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: ITTSEnumA): HResult; stdcall;
    function Select(gModeID: TGUID; out ITTSCentral: ITTSCentralA;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSENUMA}
  PITTSENUMA = ^ITTSEnumA;
  {$EXTERNALSYM ITTSEnumW}
  ITTSEnumW = interface(IUnknown)
    [SID_ITTSEnumW]
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: ITTSEnumW): HResult; stdcall;
    function Select(gModeID: TGUID; out ITTSCentral: ITTSCentralW;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSENUMW}
  PITTSENUMW = ^ITTSEnumW;
  {$EXTERNALSYM ITTSEnum}
  ITTSEnum = ITTSEnumA;

  {$EXTERNALSYM ITTSFindA}
  ITTSFindA = interface(IUnknown)
    [SID_ITTSFindA]
    function Find(var ttsInfo: TTTSModeInfoA; pttsInfoRank: PTTSModeInfoRank;
      var ttsInfoFound: TTTSModeInfoA): HResult; stdcall;
    function Select(gModeID: TGUID; out ITTSCentral: ITTSCentralA;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSFINDA}
  PITTSFINDA = ^ITTSFindA;
  {$EXTERNALSYM ITTSFindW}
  ITTSFindW = interface(IUnknown)
    [SID_ITTSFindW]
    function Find(var ttsInfo: TTTSModeInfoW; pttsInfoRank: PTTSModeInfoRank;
      var ttsInfoFound: TTTSModeInfoW): HResult; stdcall;
    function Select(gModeID: TGUID; out ITTSCentral: ITTSCentralW;
      IUnknownForAudio: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSFINDW}
  PITTSFINDW = ^ITTSFindW;
  {$EXTERNALSYM ITTSFind}
  ITTSFind = ITTSFindA;

  {$EXTERNALSYM ITTSNotifySinkA}
  ITTSNotifySinkA = interface(IUnknown)
    [SID_ITTSNotifySinkA]
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function AudioStart(qTimeStamp: QWORD): HResult; stdcall;
    function AudioStop(qTimeStamp: QWORD): HResult; stdcall;
    function Visual(qTimeStamp: QWORD; cIPAPhoneme: AnsiChar; cEnginePhoneme: AnsiChar;
      dwHints: DWORD; pTTSMouth: PTTSMOUTH): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSNOTIFYSINKA}
  PITTSNOTIFYSINKA = ^ITTSNotifySinkA;
  {$EXTERNALSYM ITTSNotifySinkW}
  ITTSNotifySinkW = interface(IUnknown)
    [SID_ITTSNotifySinkW]
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function AudioStart(qTimeStamp: QWORD): HResult; stdcall;
    function AudioStop(qTimeStamp: QWORD): HResult; stdcall;
    function Visual(qTimeStamp: QWORD; cIPAPhoneme: WideChar; cEnginePhoneme: WideChar;
      dwHints: DWORD; pTTSMouth: PTTSMOUTH): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSNOTIFYSINKW}
  PITTSNOTIFYSINKW = ^ITTSNotifySinkW;
  {$EXTERNALSYM ITTSNotifySink}
  ITTSNotifySink = ITTSNotifySinkA;

  {$EXTERNALSYM ITTSNotifySink2A}
  ITTSNotifySink2A = interface(ITTSNotifySinkA)
    [SID_ITTSNotifySinkA]
    function Error(Error: IUnknown): HResult; stdcall;
    function Warning(Warning: IUnknown): HResult; stdcall;
    function VisualFuture(dwMilliseconds: DWORD; qTimeStamp: QWORD;
      cIPAPhoneme: AnsiChar; cEnginePhoneme: AnsiChar; dwHints: DWORD;
      pTTSMouth: PTTSMouth): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSNOTIFYSINK2A}
  PITTSNOTIFYSINK2A = ^ITTSNotifySink2A;
  {$EXTERNALSYM ITTSNotifySink2W}
  ITTSNotifySink2W = interface(ITTSNotifySinkW)
    [SID_ITTSNotifySinkW]
    function Error(Error: IUnknown): HResult; stdcall;
    function Warning(Warning: IUnknown): HResult; stdcall;
    function VisualFuture(dwMilliseconds: DWORD; qTimeStamp: QWORD;
      cIPAPhoneme: WideChar; cEnginePhoneme: WideChar; dwHints: DWORD;
      pTTSMouth: PTTSMouth): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSNOTIFYSINK2W}
  PITTSNOTIFYSINK2W = ^ITTSNotifySink2W;
  {$EXTERNALSYM ITTSNotifySink2}
  ITTSNotifySink2 = ITTSNotifySink2A;


//************************************************************************
// High-Level command and control speech recognition API
//************************************************************************
const
  // VCMDNAME member lengths
  {$EXTERNALSYM VCMD_APPLEN}
  VCMD_APPLEN          =  DWORD(32);
  {$EXTERNALSYM VCMD_STATELEN}
  VCMD_STATELEN        =  VCMD_APPLEN;
  {$EXTERNALSYM VCMD_MICLEN}
  VCMD_MICLEN          =  VCMD_APPLEN;
  {$EXTERNALSYM VCMD_SPEAKERLEN}
  VCMD_SPEAKERLEN      =  VCMD_APPLEN;

  // dwFlags parameter of IVoiceCmd::MenuCreate
  {$EXTERNALSYM VCMDMC_CREATE_TEMP}
  VCMDMC_CREATE_TEMP    = $00000001;
  {$EXTERNALSYM VCMDMC_CREATE_NEW}
  VCMDMC_CREATE_NEW     = $00000002;
  {$EXTERNALSYM VCMDMC_CREATE_ALWAYS}
  VCMDMC_CREATE_ALWAYS  = $00000004;
  {$EXTERNALSYM VCMDMC_OPEN_ALWAYS}
  VCMDMC_OPEN_ALWAYS    = $00000008;
  {$EXTERNALSYM VCMDMC_OPEN_EXISTING}
  VCMDMC_OPEN_EXISTING  = $00000010;

  // dwFlags parameter of IVoiceCmd::Register
  {$EXTERNALSYM VCMDRF_NOMESSAGES}
  VCMDRF_NOMESSAGES      = 0;
  {$EXTERNALSYM VCMDRF_ALLBUTVUMETER}
  VCMDRF_ALLBUTVUMETER   = $00000001;
  {$EXTERNALSYM VCMDRF_VUMETER}
  VCMDRF_VUMETER         = $00000002;
  {$EXTERNALSYM VCMDRF_ALLMESSAGES}
  VCMDRF_ALLMESSAGES     = (VCMDRF_ALLBUTVUMETER OR VCMDRF_VUMETER);

  // dwFlags parameter of IVoiceCmd::MenuEnum
  {$EXTERNALSYM VCMDEF_DATABASE}
  VCMDEF_DATABASE        = $00000000;
  {$EXTERNALSYM VCMDEF_ACTIVE}
  VCMDEF_ACTIVE          = $00000001;
  {$EXTERNALSYM VCMDEF_SELECTED}
  VCMDEF_SELECTED        = $00000002;
  {$EXTERNALSYM VCMDEF_PERMANENT}
  VCMDEF_PERMANENT       = $00000004;
  {$EXTERNALSYM VCMDEF_TEMPORARY}
  VCMDEF_TEMPORARY       = $00000008;

  // dwFlags parameter of IVCmdMenu::Activate
  {$EXTERNALSYM VWGFLAG_ASLEEP}
  VWGFLAG_ASLEEP         = 00000001;

  // wPriority parameter of IVCmdMenu::Activate
  {$EXTERNALSYM VCMDACT_NORMAL}
  VCMDACT_NORMAL         = $8000;
  {$EXTERNALSYM VCMDACT_LOW}
  VCMDACT_LOW            = $4000;
  {$EXTERNALSYM VCMDACT_HIGH}
  VCMDACT_HIGH           = $C000;

  // dwFlags of the VCMDCOMMAND structure
  {$EXTERNALSYM VCMDCMD_VERIFY}
  VCMDCMD_VERIFY         = $00000001;
  {$EXTERNALSYM VCMDCMD_DISABLED_TEMP}
  VCMDCMD_DISABLED_TEMP  = $00000002;
  {$EXTERNALSYM VCMDCMD_DISABLED_PERM}
  VCMDCMD_DISABLED_PERM  = $00000004;
  {$EXTERNALSYM VCMDCMD_CANTRENAME}
  VCMDCMD_CANTRENAME     = $00000008;  //{SAPI 3}

  // parameter to any function that processes individual commands
  {$EXTERNALSYM VCMD_BY_POSITION}
  VCMD_BY_POSITION       = $00000001;
  {$EXTERNALSYM VCMD_BY_IDENTIFIER}
  VCMD_BY_IDENTIFIER     = $00000002;

  // values for dwAttributes field of IVCmdNotifySink::AttribChanged
  {$EXTERNALSYM IVCNSAC_AUTOGAINENABLE}
  IVCNSAC_AUTOGAINENABLE = $00000001;
  {$EXTERNALSYM IVCNSAC_ENABLED}
  IVCNSAC_ENABLED        = $00000002;
  {$EXTERNALSYM IVCNSAC_AWAKE}
  IVCNSAC_AWAKE          = $00000004;
  {$EXTERNALSYM IVCNSAC_DEVICE}
  IVCNSAC_DEVICE         = $00000008;
  {$EXTERNALSYM IVCNSAC_MICROPHONE}
  IVCNSAC_MICROPHONE     = $00000010;
  {$EXTERNALSYM IVCNSAC_SPEAKER}
  IVCNSAC_SPEAKER        = $00000020;
  {$EXTERNALSYM IVCNSAC_SRMODE}
  IVCNSAC_SRMODE         = $00000040;
  {$EXTERNALSYM IVCNSAC_THRESHOLD}
  IVCNSAC_THRESHOLD      = $00000080;
  {$EXTERNALSYM IVCNSAC_ORIGINAPP}
  IVCNSAC_ORIGINAPP      = $00010000;

  // values for dwAttributes field of IVTxtNotifySink::AttribChanged
  {$EXTERNALSYM IVTNSAC_DEVICE}
  IVTNSAC_DEVICE         = $00000001;
  {$EXTERNALSYM IVTNSAC_ENABLED}
  IVTNSAC_ENABLED        = $00000002;
  {$EXTERNALSYM IVTNSAC_SPEED}
  IVTNSAC_SPEED          = $00000004;
  {$EXTERNALSYM IVTNSAC_VOLUME}
  IVTNSAC_VOLUME         = $00000008;
  {$EXTERNALSYM IVTNSAC_TTSMODE}
  IVTNSAC_TTSMODE        = $00000010;

  // values used by IVXxxAttributes::SetMode to set the global speech
  // recognition mode
  {$EXTERNALSYM VSRMODE_DISABLED}
  VSRMODE_DISABLED       = $00000001;        //{SAPI 3}
  {$EXTERNALSYM VSRMODE_OFF}
  VSRMODE_OFF            = $00000002;        //{SAPI 3}
  {$EXTERNALSYM VSRMODE_CMDPAUSED}
  VSRMODE_CMDPAUSED      = $00000004;        //{SAPI 3}
  //  VSRMODE_CMDNOTPAUSED   = $00000008;    {added in SAPI 3 removed SAPI 4}
  {$EXTERNALSYM VSRMODE_CMDONLY}
  VSRMODE_CMDONLY        = $00000010;        //{SAPI 3}
  {$EXTERNALSYM VSRMODE_DCTONLY}
  VSRMODE_DCTONLY        = $00000020;        //{SAPI 3}
  {$EXTERNALSYM VSRMODE_CMDANDDCT}
  VSRMODE_CMDANDDCT      = $00000040;        //{SAPI 3}

type
  // voice command structure - passed to command menu functions (IVCmdMenu)
  PVCmdCommand = ^TVCmdCommand;
  {$EXTERNALSYM VCMDCOMMAND}
  VCMDCOMMAND = record
    dwSize       : DWORD;
    dwFlags      : DWORD;
    dwID         : DWORD;
    dwCommand    : DWORD;
    dwDescription: DWORD;
    dwCategory   : DWORD;
    dwCommandText: DWORD;
    dwAction     : DWORD;
    dwActionSize : DWORD;
    abData : record end;
  //  abData       : array[0..0] of BYTE;
  end;
  TVCmdCommand = VCMDCOMMAND;

  // site information structure - possible parameter to IVoiceCmd::Register  


  PVCSiteInfoA = ^TVCSiteInfoA;
  PVCSiteInfoW = ^TVCSiteInfoW;
  PVCSiteInfo = PVCSiteInfoA;
  {$EXTERNALSYM VCSITEINFOA}
  VCSITEINFOA = record
    dwAutoGainEnable : DWORD;
    dwAwakeState     : DWORD;
    dwThreshold      : DWORD;
    dwDevice         : DWORD;
    dwEnable         : DWORD;
    szMicrophone     : array[0..VCMD_MICLEN-1] of AnsiChar;
    szSpeaker        : array[0..VCMD_SPEAKERLEN-1] of AnsiChar;
    gModeID          : TGUID;
  end;
  {$EXTERNALSYM VCSITEINFOW}
  VCSITEINFOW = record
    dwAutoGainEnable : DWORD;
    dwAwakeState     : DWORD;
    dwThreshold      : DWORD;
    dwDevice         : DWORD;
    dwEnable         : DWORD;
    szMicrophone     : array[0..VCMD_MICLEN-1] of WideChar;
    szSpeaker        : array[0..VCMD_SPEAKERLEN-1] of WideChar;
    gModeID          : TGUID;
  end;
  {$EXTERNALSYM VCSITEINFO}
  VCSITEINFO = VCSITEINFOA;
  TVCSiteInfoA = VCSITEINFOA;
  TVCSiteInfoW = VCSITEINFOW;
  TVCSiteInfo = TVCSiteInfoA;



  // menu name structure
  PVCMDNameA = ^TVCMDNameA;
  PVCMDNameW = ^TVCMDNameW;
  PVCMDName = PVCMDNameA;
  {$EXTERNALSYM VCMDNAMEA}
  VCMDNAMEA = record
    szApplication    : array[0..VCMD_APPLEN-1] of AnsiChar;   // unique application name
    szState          : array[0..VCMD_STATELEN-1] of AnsiChar; // unique application state
  end;
  {$EXTERNALSYM VCMDNAMEW}
  VCMDNAMEW = record
    szApplication    : array[0..VCMD_APPLEN-1] of WideChar;   // unique application name
    szState          : array[0..VCMD_STATELEN-1] of WideChar; // unique application state
  end;
  {$EXTERNALSYM VCMDNAME}
  VCMDNAME = VCMDNAMEA;
  TVCMDNameA = VCMDNAMEA;
  TVCMDNameW = VCMDNAMEW;
  TVCMDName = TVCMDNameA;

  {$EXTERNALSYM IVCmdNotifySinkA}
  IVCmdNotifySinkA = interface(IUnknown)
    [SID_IVCmdNotifySinkA]
    function CommandRecognize(dwID: DWORD; pvCmdName: PVCmdNameA; pdwFlags: PDWORD;
      dwActionSize: DWORD; pAction: pointer; dwNumLists: DWORD;
      pszListValues: PAnsiChar; pszCommand: PAnsiChar): HResult; stdcall;
    function CommandOther(pName: PVCmdNameA; pszCommand: PAnsiChar): HResult; stdcall;
    function CommandStart: HResult; stdcall;
    function MenuActivate(pName: PVCmdNameA; bActive: BOOL): HResult; stdcall;
    function UtteranceBegin: HResult; stdcall;
    function UtteranceEnd: HResult; stdcall;
    function VUMeter(wLevel: WORD): HResult; stdcall;
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Interference(dwType: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDNOTIFYSINKA}
  PIVCMDNOTIFYSINKA = ^IVCmdNotifySinkA;
  {$EXTERNALSYM IVCmdNotifySinkW}
  IVCmdNotifySinkW = interface(IUnknown)
    [SID_IVCmdNotifySinkW]
    function CommandRecognize(dwID: DWORD; pvCmdName: PVCmdNameW; pdwFlags: PDWORD;
      dwActionSize: DWORD; pAction: pointer; dwNumLists: DWORD;
      pszListValues: PWideChar; pszCommand: PWideChar): HResult; stdcall;
    function CommandOther(pName: PVCmdNameW; pszCommand: PWideChar): HResult; stdcall;
    function CommandStart: HResult; stdcall;
    function MenuActivate(pName: PVCmdNameW; bActive: BOOL): HResult; stdcall;
    function UtteranceBegin: HResult; stdcall;
    function UtteranceEnd: HResult; stdcall;
    function VUMeter(wLevel: WORD): HResult; stdcall;
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Interference(dwType: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDNOTIFYSINKW}
  PIVCMDNOTIFYSINKW = ^IVCmdNotifySinkW;
  {$EXTERNALSYM IVCmdNotifySink}
  IVCmdNotifySink = IVCmdNotifySinkA;

  {$EXTERNALSYM IVCmdEnumA}
  IVCmdEnumA = interface(IUnknown)
    [SID_IVCmdEnumA]
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: IVCmdEnumA): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDENUMA}
  PIVCMDENUMA = ^IVCmdEnumA;
  {$EXTERNALSYM IVCmdEnumW}
  IVCmdEnumW = interface(IUnknown)
    [SID_IVCmdEnumW]
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: IVCmdEnumW): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDENUMW}
  PIVCMDENUMW = ^IVCmdEnumW;
  {$EXTERNALSYM IVCmdEnum}
  IVCmdEnum = IVCmdEnumA;

  {$EXTERNALSYM IEnumSRShareA}
  IEnumSRShareA = interface(IUnknown)
    [SID_IEnumSRShareA]
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumSRShareA): HResult; stdcall;
    function New(dwDeviceID: DWORD; gModeID: TGUID;
      var ISRCentral: ISRCentralA; var qwInstanceID: QWORD): HResult; stdcall;
    function Share(qwInstanceID: QWORD; var ISRCentral: ISRCentralA): HResult; stdcall;
    function Detach(qwInstanceID: QWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIENUMSRSHAREA}
  PIENUMSRSHAREA = ^IEnumSRShareA;
  {$EXTERNALSYM IEnumSRShareW}
  IEnumSRShareW = interface(IUnknown)
    [SID_IEnumSRShareW]
    function Next(celt: ULONG; var rgelt; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumSRShareW): HResult; stdcall;
    function New(dwDeviceID: DWORD; gModeID: TGUID;
      var ISRCentral: ISRCentralW; var qwInstanceID: QWORD): HResult; stdcall;
    function Share(qwInstanceID: QWORD; var ISRCentral: ISRCentralW): HResult; stdcall;
    function Detach(qwInstanceID: QWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIENUMSRSHAREW}
  PIENUMSRSHAREW = ^IEnumSRShareW;
  {$EXTERNALSYM IEnumSRShare}
  IEnumSRShare = IEnumSRShareA;

  {$EXTERNALSYM IVCmdMenuA}
  IVCmdMenuA = interface(IUnknown)
    [SID_IVCmdMenuA]
    function Activate(hWndListening: HWND; dwFlags: DWORD): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function TrainMenuDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function Num(var dwNumCmd: DWORD): HResult; stdcall;
    function Get(dwCmdStart: DWORD; dwCmdNum: DWORD; dwFlag: DWORD;
      pdData: PSDATA; var dwCmdNumOut: DWORD): HResult; stdcall;
    function DoSet(dwCmdStart: DWORD; dwCmdNum: DWORD; dwFlag: DWORD;
      dData: SDATA): HResult; stdcall;
    function Add(dwCmdNum: DWORD; dData: SDATA; var dwCmdStart: DWORD): HResult; stdcall;
    function Remove(dwCmdStart: DWORD; dwCmdNum: DWORD; dwFlag: DWORD): HResult; stdcall;
    function ListGet(pszList: PAnsiChar; pdList: PSDATA; var dwListNum: DWORD): HResult; stdcall;
    function ListSet(pszList: PAnsiChar; dwListNum: DWORD; dList: SDATA): HResult; stdcall;
    function EnableItem(dwEnable: DWORD; dwCmdNum: DWORD; dwFlag: DWORD): HResult; stdcall;
    function SetItem(dwEnable: DWORD; dwCmdNum: DWORD; dwFlag: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDMENUA}
  PIVCMDMENUA = ^IVCmdMenuA;
  {$EXTERNALSYM IVCmdMenuW}
  IVCmdMenuW = interface(IUnknown)
    [SID_IVCmdMenuW]
    function Activate(hWndListening: HWND; dwFlags: DWORD): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function TrainMenuDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function Num(var dwNumCmd: DWORD): HResult; stdcall;
    function Get(dwCmdStart: DWORD; dwCmdNum: DWORD; dwFlag: DWORD;
      pdData: PSDATA; var dwCmdNumOut: DWORD): HResult; stdcall;
    function DoSet(dwCmdStart: DWORD; dwCmdNum: DWORD; dwFlag: DWORD;
      dData: SDATA): HResult; stdcall;
    function Add(dwCmdNum: DWORD; dData: SDATA; var dwCmdStart: DWORD): HResult; stdcall;
    function Remove(dwCmdStart: DWORD; dwCmdNum: DWORD; dwFlag: DWORD): HResult; stdcall;
    function ListGet(pszList: PWideChar; pdList: PSDATA; var dwListNum: DWORD): HResult; stdcall;
    function ListSet(pszList: PWideChar; dwListNum: DWORD; dList: SDATA): HResult; stdcall;
    function EnableItem(dwEnable: DWORD; dwCmdNum: DWORD; dwFlag: DWORD): HResult; stdcall;
    function SetItem(dwEnable: DWORD; dwCmdNum: DWORD; dwFlag: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDMENUW}
  PIVCMDMENUW = ^IVCmdMenuW;
  {$EXTERNALSYM IVCmdMenu}
  IVCmdMenu = IVCmdMenuA;

  {$EXTERNALSYM IVoiceCmdA}
  IVoiceCmdA = interface(IUnknown)
    [SID_IVoiceCmdA]
    function Register(pszSite: PAnsiChar; pNotifyInterface: IUnknown;
      IIDNotifyInterface: TGUID; dwFlags: DWORD; pSiteInfo: PVCSiteInfoA): HResult; stdcall;
    function MenuEnum(dwFlags: DWORD; pszApplicationFilter: PAnsiChar;
      pszStateFilter: PAnsiChar; var IVCmdEnum: IVCmdEnumA): HResult; stdcall;
    function MenuCreate(pName: PVCmdNameA; pLanguage: PLanguageA;
      dwFlags: DWORD; var IVCmdMenu: IVCmdMenuA): HResult; stdcall;
    function MenuDelete(pName: PVCmdNameA): HResult; stdcall;
    function CmdMimic(pMenu: PVCmdNameA; pszCommand: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVOICECMDA}
  PIVOICECMDA = ^IVoiceCmdA;
  {$EXTERNALSYM IVoiceCmdW}
  IVoiceCmdW = interface(IUnknown)
    [SID_IVoiceCmdW]
    function Register(pszSite: PWideChar; pNotifyInterface: IUnknown;
      IIDNotifyInterface: TGUID; dwFlags: DWORD; pSiteInfo: PVCSiteInfoW): HResult; stdcall;
    function MenuEnum(dwFlags: DWORD; pszApplicationFilter: PWideChar;
      pszStateFilter: PWideChar; var IVCmdEnum: IVCmdEnumW): HResult; stdcall;
    function MenuCreate(pName: PVCmdNameW; pLanguage: PLanguageW;
      dwFlags: DWORD; var IVCmdMenu: IVCmdMenuW): HResult; stdcall;
    function MenuDelete(pName: PVCmdNameW): HResult; stdcall;
    function CmdMimic(pMenu: PVCmdNameW; pszCommand: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVOICECMDW}
  PIVOICECMDW = ^IVoiceCmdW;
  {$EXTERNALSYM IVoiceCmd}
  IVoiceCmd = IVoiceCmdA;

  {$EXTERNALSYM IVCmdAttributesA}
  IVCmdAttributesA = interface(IUnknown)
    [SID_IVCmdAttributesA]
    function AutoGainEnableGet(var dwAutoGain: DWORD): HResult; stdcall;
    function AutoGainEnableSet(dwAutoGain: DWORD): HResult; stdcall;
    function AwakeStateGet(var dwAwake: DWORD): HResult; stdcall;
    function AwakeStateSet(dwAwake: DWORD): HResult; stdcall;
    function ThresholdGet(var dwThreshold: DWORD): HResult; stdcall;
    function ThresholdSet(dwThreshold: DWORD): HResult; stdcall;
    function DeviceGet(var dwDeviceID: DWORD): HResult; stdcall;
    function DeviceSet(dwDeviceID: DWORD): HResult; stdcall;
    function EnabledGet(var dwEnabled: DWORD): HResult; stdcall;
    function EnabledSet(dwEnabled: DWORD): HResult; stdcall;
    function MicrophoneGet(pszMicrophone: PAnsiChar; dwSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function MicrophoneSet(pszMicrophone: PAnsiChar): HResult; stdcall;
    function SpeakerGet(pszSpeaker: PAnsiChar; dwSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function SpeakerSet(pszSpeaker: PAnsiChar): HResult; stdcall;
    function SRModeGet(var gMode: TGUID): HResult; stdcall;
    function SRModeSet(gMode: TGUID): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDATTRIBUTESA}
  PIVCMDATTRIBUTESA = ^IVCmdAttributesA;
  {$EXTERNALSYM IVCmdAttributesW}
  IVCmdAttributesW = interface(IUnknown)
    [SID_IVCmdAttributesW]
    function AutoGainEnableGet(var dwAutoGain: DWORD): HResult; stdcall;
    function AutoGainEnableSet(dwAutoGain: DWORD): HResult; stdcall;
    function AwakeStateGet(var dwAwake: DWORD): HResult; stdcall;
    function AwakeStateSet(dwAwake: DWORD): HResult; stdcall;
    function ThresholdGet(var dwThreshold: DWORD): HResult; stdcall;
    function ThresholdSet(dwThreshold: DWORD): HResult; stdcall;
    function DeviceGet(var dwDeviceID: DWORD): HResult; stdcall;
    function DeviceSet(dwDeviceID: DWORD): HResult; stdcall;
    function EnabledGet(var dwEnabled: DWORD): HResult; stdcall;
    function EnabledSet(dwEnabled: DWORD): HResult; stdcall;
    function MicrophoneGet(pszMicrophone: PWideChar; dwSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function MicrophoneSet(pszMicrophone: PWideChar): HResult; stdcall;
    function SpeakerGet(pszSpeaker: PWideChar; dwSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function SpeakerSet(pszSpeaker: PWideChar): HResult; stdcall;
    function SRModeGet(var gMode: TGUID): HResult; stdcall;
    function SRModeSet(gMode: TGUID): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDATTRIBUTESW}
  PIVCMDATTRIBUTESW = ^IVCmdAttributesW;
  {$EXTERNALSYM IVCmdAttributes}
  IVCmdAttributes = IVCmdAttributesA;

  {$EXTERNALSYM IVCmdDialogsA}
  IVCmdDialogsA = interface(IUnknown)
    [SID_IVCmdDialogsA]
    function AboutDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainGeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainMicDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDDIALOGSA}
  PIVCMDDIALOGSA = ^IVCmdDialogsA;
  {$EXTERNALSYM IVCmdDialogsW}
  IVCmdDialogsW = interface(IUnknown)
    [SID_IVCmdDialogsW]
    function AboutDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainGeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainMicDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVCMDDIALOGSW}
  PIVCMDDIALOGSW = ^IVCmdDialogsW;
  {$EXTERNALSYM IVCmdDialogs}
  IVCmdDialogs = IVCmdDialogsA;

//************************************************************************
// High-Level dictation speech recognition API
//************************************************************************
const
  {$EXTERNALSYM VDCT_TOPICNAMELEN}
  VDCT_TOPICNAMELEN    = 32;
  // bit flags for the dwReason field of IVDctNotifySink.TextChanged
  {$EXTERNALSYM VDCT_TEXTADDED}
  VDCT_TEXTADDED       = $00000001;
  {$EXTERNALSYM VDCT_TEXTREMOVED}
  VDCT_TEXTREMOVED     = $00000002;
  {$EXTERNALSYM VDCT_TEXTREPLACED}
  VDCT_TEXTREPLACED    = $00000004;
  // bit flags for the dwReason field of IVDctText.TextRemove/TextSet
  {$EXTERNALSYM VDCT_TEXTCLEAN}
  VDCT_TEXTCLEAN       = $00010000;
  {$EXTERNALSYM VDCT_TEXTKEEPRESULTS}
  VDCT_TEXTKEEPRESULTS = $00020000;
  // bit flags for dwFlags of IVDctGUI.FlagsSet
  {$EXTERNALSYM VDCTGUIF_VISIBLE}
  VDCTGUIF_VISIBLE     = $00000001;
  {$EXTERNALSYM VDCTGUIF_DONTMOVE}
  VDCTGUIF_DONTMOVE    = $00000002;
  {$EXTERNALSYM VDCTGUIF_ADDWORD}
  VDCTGUIF_ADDWORD     = $00000004;
  // bit flags for dwFX of IVDctText.FX
  {$EXTERNALSYM VDCTFX_CAPFIRST}
  VDCTFX_CAPFIRST      = $00000001;
  {$EXTERNALSYM VDCTFX_LOWERFIRST}
  VDCTFX_LOWERFIRST    = $00000002;
  {$EXTERNALSYM VDCTFX_TOGGLEFIRST}
  VDCTFX_TOGGLEFIRST   = $00000003;
  {$EXTERNALSYM VDCTFX_CAPALL}
  VDCTFX_CAPALL        = $00000004;
  {$EXTERNALSYM VDCTFX_LOWERALL}
  VDCTFX_LOWERALL      = $00000005;
  {$EXTERNALSYM VDCTFX_REMOVESPACES}
  VDCTFX_REMOVESPACES  = $00000006;            //SAPI 4
  {$EXTERNALSYM VDCTFX_KEEPONLYFIRSTLETTER}
  VDCTFX_KEEPONLYFIRSTLETTER  = $00000007;     //SAPI 4

type

  //site information structure - used for IVoiceDictation.SiteInfoGet/Set
  PVDSiteInfoA = ^TVDSiteInfoA;
  PVDSiteInfoW = ^TVDSiteInfoW;
  PVDSiteInfo = PVDSiteInfoA;
  {$EXTERNALSYM VDSITEINFOA}
  VDSITEINFOA = record
    dwAutoGainEnable: DWORD;
    dwAwakeState: DWORD;
    dwThreshold: DWORD;
    dwDevice: DWORD;
    dwEnable: DWORD;
    szMicrophone: array[0..VCMD_MICLEN-1] of AnsiChar;
    szSpeaker: array[0..VCMD_SPEAKERLEN-1] of AnsiChar;
    gModeID: TGUID;
  end;
  {$EXTERNALSYM VDSITEINFOW}
  VDSITEINFOW = record
    dwAutoGainEnable: DWORD;
    dwAwakeState: DWORD;
    dwThreshold: DWORD;
    dwDevice: DWORD;
    dwEnable: DWORD;
    szMicrophone: array[0..VCMD_MICLEN-1] of WideChar;
    szSpeaker: array[0..VCMD_SPEAKERLEN-1] of WideChar;
    gModeID: TGUID;
  end;
  {$EXTERNALSYM VDSITEINFO}
  VDSITEINFO = VDSITEINFOA;
  TVDSiteInfoA = VDSITEINFOA;
  TVDSiteInfoW = VDSITEINFOW;
  TVDSiteInfo = TVDSiteInfoA;


  // topic structure used by the IVoiceDictation object
  PVDctTopicA = ^TVDctTopicA;
  PVDctTopicW = ^TVDctTopicW;
  PVDctTopic = PVDctTopicA;
  {$EXTERNALSYM VDCTTOPICA}
  VDCTTOPICA = record
    szTopic: array[0..VDCT_TOPICNAMELEN-1] of AnsiChar;
    language: TLanguageA;
  end;
  {$EXTERNALSYM VDCTTOPICW}
  VDCTTOPICW = record
    szTopic: array[0..VDCT_TOPICNAMELEN-1] of WideChar;
    language: TLanguageW;
  end;
  {$EXTERNALSYM VDCTTOPIC}
  VDCTTOPIC = VDCTTOPICA;
  TVDctTopicA = VDCTTOPICA;
  TVDctTopicW = VDCTTOPICW;
  TVDctTopic = TVDctTopicA;


  // memory maintenance structure used by MemoryGet/Set in IVDctAttributes
  PVDctMemory = ^TVDctMemory;
  {$EXTERNALSYM VDCTMEMORY}
  VDCTMEMORY = record
    dwMaxRAM: DWORD;
    dwMaxTime: DWORD;
    dwMaxWords: DWORD;
    fKeepAudio: Bool;
    fKeepCorrection: Bool;
    fKeepEval: Bool;
  end;
  TVDctMemory = VDCTMEMORY;

  // bookmark definition
  PVDCTBookMark = ^TVDCTBookMark;
  {$EXTERNALSYM VDCTBOOKMARK}
  VDCTBOOKMARK = record
    dwID: DWORD;
    dwPosn: DWORD;
  end;
  TVDCTBookMark = VDCTBOOKMARK;

  {$EXTERNALSYM IVDctNotifySinkA}
  IVDctNotifySinkA = interface(IUnknown)
    [SID_IVDctNotifySinkA]
    function CommandBuiltIn(pszCommand: PAnsiChar): HResult; stdcall;
    function CommandOther(pszCommand: PAnsiChar): HResult; stdcall;
    function CommandRecognize(dwID: DWord; pdwFlags: PDWord; dwActionSize: DWORD;
      pAction: Pointer; pszCommand: PAnsiChar): HResult; stdcall;
    function TextSelChanged: HResult; stdcall;
    function TextChanged(dwReason: DWORD): HResult; stdcall;
    function TextBookmarkChanged(dwID: DWORD): HResult; stdcall;
    function PhraseStart: HResult; stdcall;
    function PhraseFinish(dwFlags: DWORD; pSRPhrase: PSRPhraseA): HResult; stdcall;
    function PhraseHypothesis(dwFlags: DWORD; pSRPhrase: PSRPhraseA): HResult; stdcall;
    function UtteranceBegin: HResult; stdcall;
    function UtteranceEnd: HResult; stdcall;
    function VUMeter(wLevel: WORD): HResult; stdcall;
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Interference(dwType: DWORD): HResult; stdcall;
    function Training(dwTrain: DWORD): HResult; stdcall;
    function Dictating(pszApp: PAnsiChar; fDictating: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTNOTIFYSINKA}
  PIVDCTNOTIFYSINKA = ^IVDctNotifySinkA;
  {$EXTERNALSYM IVDctNotifySinkW}
  IVDctNotifySinkW = interface(IUnknown)
    [SID_IVDctNotifySinkW]
    function CommandBuiltIn(pszCommand: PWideChar): HResult; stdcall;
    function CommandOther(pszCommand: PWideChar): HResult; stdcall;
    function CommandRecognize(dwID: DWord; pdwFlags: PDWord; dwActionSize: DWORD;
      pAction: Pointer; pszCommand: PWideChar): HResult; stdcall;
    function TextSelChanged: HResult; stdcall;
    function TextChanged(dwReason: DWORD): HResult; stdcall;
    function TextBookmarkChanged(dwID: DWORD): HResult; stdcall;
    function PhraseStart: HResult; stdcall;
    function PhraseFinish(dwFlags: DWORD; pSRPhrase: PSRPhraseW): HResult; stdcall;
    function PhraseHypothesis(dwFlags: DWORD; pSRPhrase: PSRPhraseW): HResult; stdcall;
    function UtteranceBegin: HResult; stdcall;
    function UtteranceEnd: HResult; stdcall;
    function VUMeter(wLevel: WORD): HResult; stdcall;
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Interference(dwType: DWORD): HResult; stdcall;
    function Training(dwTrain: DWORD): HResult; stdcall;
    function Dictating(pszApp: PWideChar; fDictating: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTNOTIFYSINKW}
  PIVDCTNOTIFYSINKW = ^IVDctNotifySinkW;
  {$EXTERNALSYM IVDctNotifySink}
  IVDctNotifySink = IVDctNotifySinkA;

  {$EXTERNALSYM IVDctNotifySink2A}
  IVDctNotifySink2A = interface(IVDctNotifySinkA)
    [SID_IVDctNotifySink2A]
    function Error(Error: IUnknown): HResult; stdcall;
    function Warning(Warning: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTNOTIFYSINK2A}
  PIVDCTNOTIFYSINK2A = ^IVDctNotifySink2A;
  {$EXTERNALSYM IVDctNotifySink2W}
  IVDctNotifySink2W = interface(IVDctNotifySinkW)
    [SID_IVDctNotifySink2W]
    function Error(Error: IUnknown): HResult; stdcall;
    function Warning(Warning: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTNOTIFYSINK2W}
  PIVDCTNOTIFYSINK2W = ^IVDctNotifySink2W;
  {$EXTERNALSYM IVDctNotifySink2}
  IVDctNotifySink2 = IVDctNotifySink2A;

  {$EXTERNALSYM IVoiceDictationA}
  IVoiceDictationA = interface(IUnknown)
    [SID_IVoiceDictationA]
    function Register(pszApplication, pszTopic: PAnsiChar; ISessionData: IStorage;
      pszSite: PAnsiChar; NotifyInterface: IVDctNotifySinkA;
      IIDNotifyInterface: TGUID; dwFlags: DWORD): HResult; stdcall;
    function SiteInfoGet(pszSite: PAnsiChar; pSiteInfo: PVDSiteInfoA): HResult; stdcall;
    function SiteInfoSet(pszSite: PAnsiChar; pSiteInfo: PVDSiteInfoA): HResult; stdcall;
    function SessionSerialize(ISessionData: IStorage): HResult; stdcall;
    function SessionDeserialize(ISessionData: IStorage): HResult; stdcall;
    function TopicEnum(pData: PSDATA): HResult; stdcall;
    function TopicAddString(pszTopic: PAnsiChar; pLanguage: PLANGUAGEA): HResult; stdcall;
    function TopicRemove(pszTopic: PAnsiChar): HResult; stdcall;
    function TopicSerialize(ITopicData: IStorage): HResult; stdcall;
    function TopicDeserialize(ITopicData: IStorage): HResult; stdcall;
    function Activate(hWnd: HWND): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function TopicAddGrammar(pszTopic: PAnsiChar; sData: SDATA): HResult; stdcall;
    function CFGSet(Lang: LANGID; pszTopic: PAnsiChar; pszCFG: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVOICEDICTATIONA}
  PIVOICEDICTATIONA = ^IVoiceDictationA;
  {$EXTERNALSYM IVoiceDictationW}
  IVoiceDictationW = interface(IUnknown)
    [SID_IVoiceDictationW]
    function Register(pszApplication, pszTopic: PWideChar; ISessionData: IStorage;
      pszSite: PWideChar; NotifyInterface: IVDctNotifySinkW;
      IIDNotifyInterface: TGUID; dwFlags: DWORD): HResult; stdcall;
    function SiteInfoGet(pszSite: PWideChar; pSiteInfo: PVDSiteInfoW): HResult; stdcall;
    function SiteInfoSet(pszSite: PWideChar; pSiteInfo: PVDSiteInfoW): HResult; stdcall;
    function SessionSerialize(ISessionData: IStorage): HResult; stdcall;
    function SessionDeserialize(ISessionData: IStorage): HResult; stdcall;
    function TopicEnum(pData: PSDATA): HResult; stdcall;
    function TopicAddString(pszTopic: PWideChar; pLanguage: PLANGUAGEW): HResult; stdcall;
    function TopicRemove(pszTopic: PWideChar): HResult; stdcall;
    function TopicSerialize(ITopicData: IStorage): HResult; stdcall;
    function TopicDeserialize(ITopicData: IStorage): HResult; stdcall;
    function Activate(hWnd: HWND): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function TopicAddGrammar(pszTopic: PWideChar; sData: SDATA): HResult; stdcall;
    function CFGSet(Lang: LANGID; pszTopic: PWideChar; pszCFG: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVOICEDICTATIONW}
  PIVOICEDICTATIONW = ^IVoiceDictationW;
  {$EXTERNALSYM IVoiceDictation}
  IVoiceDictation = IVoiceDictationA;

  {$EXTERNALSYM IVDctTextA}
  IVDctTextA = interface(IUnknown)
    [SID_IVDctTextA]
    function Lock: HResult; stdcall;
    function UnLock: HResult; stdcall;
    function TextGet(dwStart, dwNumChars: DWORD; pData: PSDATA): HResult; stdcall;
    function TextSet(dwStart, dwNumChars: DWORD; pszBuf: PAnsiChar; dwReason: DWORD): HResult; stdcall;
    function TextMove(dwStart, dwNumChars, dwMoveTo, dwReason: DWORD): HResult; stdcall;
    function TextRemove(dwStart, dwNumChars, dwReason: DWORD): HResult; stdcall;
    function TextSelSet(dwStart, dwNumChars: DWORD): HResult; stdcall;
    function TextSelGet(var dwStart, dwNumChars: DWORD): HResult; stdcall;
    function TextLengthGet(var dwLength: DWORD): HResult; stdcall;
    function GetChanges(var dwNewStart, dwNewEnd, dwOldStart, dwOldEnd: DWORD): HResult; stdcall;
    function BookmarkAdd(pBookMark: PVDCTBOOKMARK): HResult; stdcall;  // This is not a var, its an in parameter
    function BookmarkRemove(dwID: DWORD): HResult; stdcall;
    function BookmarkMove(dwID, dwNewPosn: DWORD): HResult; stdcall;
    function BookmarkQuery(dwID: DWORD; pBookMark: PVDCTBookMark): HResult; stdcall;
    function BookmarkEnum(dwStart, dwNumChars: DWORD; var pBookMark: PVDCTBookMark;
      var dwNumMarks: DWORD): HResult; stdcall;
    function Hint(pszHint: PAnsiChar): HResult; stdcall;
    function Words(pszWords: PAnsiChar): HResult; stdcall;
    function ResultsGet(dwStart, dwNumChars: DWORD;
      var dwPhraseStart, dwPhraseNumChars: DWORD;
      var IUnkPhraseResults: IUnknown): HResult; stdcall;
    function FX(dwFX: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTTEXTA}
  PIVDCTTEXTA = ^IVDctTextA;
  {$EXTERNALSYM IVDctTextW}
  IVDctTextW = interface(IUnknown)
    [SID_IVDctTextW]
    function Lock: HResult; stdcall;
    function UnLock: HResult; stdcall;
    function TextGet(dwStart, dwNumChars: DWORD; pData: PSDATA): HResult; stdcall;
    function TextSet(dwStart, dwNumChars: DWORD; pszBuf: PWideChar; dwReason: DWORD): HResult; stdcall;
    function TextMove(dwStart, dwNumChars, dwMoveTo, dwReason: DWORD): HResult; stdcall;
    function TextRemove(dwStart, dwNumChars, dwReason: DWORD): HResult; stdcall;
    function TextSelSet(dwStart, dwNumChars: DWORD): HResult; stdcall;
    function TextSelGet(var dwStart, dwNumChars: DWORD): HResult; stdcall;
    function TextLengthGet(var dwLength: DWORD): HResult; stdcall;
    function GetChanges(var dwNewStart, dwNewEnd, dwOldStart, dwOldEnd: DWORD): HResult; stdcall;
    function BookmarkAdd(pBookMark: PVDCTBOOKMARK): HResult; stdcall;  // This is not a var, its an in parameter
    function BookmarkRemove(dwID: DWORD): HResult; stdcall;
    function BookmarkMove(dwID, dwNewPosn: DWORD): HResult; stdcall;
    function BookmarkQuery(dwID: DWORD; pBookMark: PVDCTBookMark): HResult; stdcall;
    function BookmarkEnum(dwStart, dwNumChars: DWORD; var pBookMark: PVDCTBookMark;
      var dwNumMarks: DWORD): HResult; stdcall;
    function Hint(pszHint: PWideChar): HResult; stdcall;
    function Words(pszWords: PWideChar): HResult; stdcall;
    function ResultsGet(dwStart, dwNumChars: DWORD;
      var dwPhraseStart, dwPhraseNumChars: DWORD;
      var IUnkPhraseResults: IUnknown): HResult; stdcall;
    function FX(dwFX: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTTEXTW}
  PIVDCTTEXTW = ^IVDctTextW;
  {$EXTERNALSYM IVDctText}
  IVDctText = IVDctTextA;

  {$EXTERNALSYM IVDctText2A}
  IVDctText2A = interface(IVDctTextA)                               //SAPI 4
    [SID_IVDctText2A]
    function ITNApply(dwStart, dwNumChars: DWORD): HResult; stdcall;
    function ITNExpand(dwStart, dwNumChars: DWORD): HResult; stdcall;
    function ResultsGet2(dwStart, dwNumChars: DWORD;
      var dwPhraseStart, dwPhraseNumChars: DWORD;
      var IUnkPhraseResults: IUnknown;
      var dwNodeLeft, dwNodeRight: DWORD): HResult; stdcall;
    function ResultsSet(dwStart, dwNumChars: DWORD;
      IUnkPhraseResults: IUnknown;
      dwNodeLeft, dwNodeRight: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTTEXT2A}
  PIVDCTTEXT2A = ^IVDctText2A;
  {$EXTERNALSYM IVDctText2W}
  IVDctText2W = interface(IVDctTextW)                               //SAPI 4
    [SID_IVDctText2W]
    function ITNApply(dwStart, dwNumChars: DWORD): HResult; stdcall;
    function ITNExpand(dwStart, dwNumChars: DWORD): HResult; stdcall;
    function ResultsGet2(dwStart, dwNumChars: DWORD;
      var dwPhraseStart, dwPhraseNumChars: DWORD;
      var IUnkPhraseResults: IUnknown;
      var dwNodeLeft, dwNodeRight: DWORD): HResult; stdcall;
    function ResultsSet(dwStart, dwNumChars: DWORD;
      IUnkPhraseResults: IUnknown;
      dwNodeLeft, dwNodeRight: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTTEXT2W}
  PIVDCTTEXT2W = ^IVDctText2W;
  {$EXTERNALSYM IVDctText2}
  IVDctText2 = IVDctText2A;

  {$EXTERNALSYM IVDctTextCache}
  IVDctTextCache = interface(IUnknown)
    [SID_IVDctTextCache]
    function CopyToBin(dwStart, dwNumChars, dwBin: DWORD): HResult; stdcall;
    function CopyToMemory(dwStart, dwNumChars: DWORD; var pMem: pointer;
      var dwMemSize: DWORD; var aUnk: IUnknown; var dwUnkSize: DWORD): HResult; stdcall;
    function PasteFromBin(dwStart, dwNumChars, dwBin: DWORD): HResult; stdcall;
    function PasteFromMemory(dwStart, dwNumChars: DWORD; pMem: Pointer;
      dwMemSize: DWORD; var aUnk: IUnknown; dwUnkSize: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTTEXTCACHE}
  PIVDCTTEXTCACHE = ^IVDctTextCache;

  {$EXTERNALSYM IVDctInvTextNormA}
  IVDctInvTextNormA = interface(IUnknown)
    [SID_IVDctInvTextNormA]
    function OptionsEnum(var pOptions: PAnsiChar; var dwSize: DWORD): HResult; stdcall;
    function OptionsGet(pszOptionName: PAnsiChar; var fOn: BOOL): HResult; stdcall;
    function OptionsSet(pszOptionName: PAnsiChar; fOn: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTINVTEXTNORMA}
  PIVDCTINVTEXTNORMA = ^IVDctInvTextNormA;
  {$EXTERNALSYM IVDctInvTextNormW}
  IVDctInvTextNormW = interface(IUnknown)
    [SID_IVDctInvTextNormW]
    function OptionsEnum(var pOptions: PWideChar; var dwSize: DWORD): HResult; stdcall;
    function OptionsGet(pszOptionName: PWideChar; var fOn: BOOL): HResult; stdcall;
    function OptionsSet(pszOptionName: PWideChar; fOn: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTINVTEXTNORMW}
  PIVDCTINVTEXTNORMW = ^IVDctInvTextNormW;
  {$EXTERNALSYM IVDctInvTextNorm}
  IVDctInvTextNorm = IVDctInvTextNormA;

  {$EXTERNALSYM IVDctAttributesA}
  IVDctAttributesA = interface(IUnknown)
    [SID_IVDctAttributesA]
    function AutoGainEnableGet(var dwAutoGain: DWORD): HResult; stdcall;
    function AutoGainEnableSet(dwAutoGain: DWORD): HResult; stdcall;
    function AwakeStateGet(var dwAwake: DWORD): HResult; stdcall;
    function AwakeStateSet(dwAwake: DWORD): HResult; stdcall;
    function ThresholdGet(var dwThreshold: DWORD): HResult; stdcall;
    function ThresholdSet(dwThreshold: DWORD): HResult; stdcall;
    function EchoGet(var fEchoOn: BOOL): HResult; stdcall;
    function EchoSet(fEchoOn: BOOL): HResult; stdcall;
    function EnergyFloorGet(var wEnergy: WORD): HResult; stdcall;
    function EnergyFloorSet(wEnergy: WORD): HResult; stdcall;
    function RealTimeGet(var dwRealTime: DWORD): HResult; stdcall;
    function RealTimeSet(dwRealTime: DWORD): HResult; stdcall;
    function TimeOutGet(var dwIncomplete, dwComplete: DWORD): HResult; stdcall;
    function TimeOutSet(dwIncomplete, dwComplete: DWORD): HResult; stdcall;
    function ModeGet(var dwMode: DWORD): HResult; stdcall;
    function ModeSet(dwMode: DWORD): HResult; stdcall;
    function MemoryGet(var DctMemory: TVDCTMemory): HResult; stdcall;
    function MemorySet(var DctMemory: TVDCTMemory): HResult; stdcall;
    function IsAnyoneDictating(hWndParent: HWND; pszName: PAnsiChar;
      dwNameSize: DWORD; var dwNameNeeded: DWORD): HResult; stdcall;
    function MicrophoneGet(pszMicrophone: PAnsiChar; dwMicrophoneSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function MicrophoneSet(pszMicrophone: PAnsiChar): HResult; stdcall;
    function SpeakerGet(pszSpeaker: PAnsiChar; dwSpeakerSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function SpeakerSet(pszSpeaker: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTATTRIBUTESA}
  PIVDCTATTRIBUTESA = ^IVDctAttributesA;
  {$EXTERNALSYM IVDctAttributesW}
  IVDctAttributesW = interface(IUnknown)
    [SID_IVDctAttributesW]
    function AutoGainEnableGet(var dwAutoGain: DWORD): HResult; stdcall;
    function AutoGainEnableSet(dwAutoGain: DWORD): HResult; stdcall;
    function AwakeStateGet(var dwAwake: DWORD): HResult; stdcall;
    function AwakeStateSet(dwAwake: DWORD): HResult; stdcall;
    function ThresholdGet(var dwThreshold: DWORD): HResult; stdcall;
    function ThresholdSet(dwThreshold: DWORD): HResult; stdcall;
    function EchoGet(var fEchoOn: BOOL): HResult; stdcall;
    function EchoSet(fEchoOn: BOOL): HResult; stdcall;
    function EnergyFloorGet(var wEnergy: WORD): HResult; stdcall;
    function EnergyFloorSet(wEnergy: WORD): HResult; stdcall;
    function RealTimeGet(var dwRealTime: DWORD): HResult; stdcall;
    function RealTimeSet(dwRealTime: DWORD): HResult; stdcall;
    function TimeOutGet(var dwIncomplete, dwComplete: DWORD): HResult; stdcall;
    function TimeOutSet(dwIncomplete, dwComplete: DWORD): HResult; stdcall;
    function ModeGet(var dwMode: DWORD): HResult; stdcall;
    function ModeSet(dwMode: DWORD): HResult; stdcall;
    function MemoryGet(var DctMemory: TVDCTMemory): HResult; stdcall;
    function MemorySet(var DctMemory: TVDCTMemory): HResult; stdcall;
    function IsAnyoneDictating(hWndParent: HWND; pszName: PWideChar;
      dwNameSize: DWORD; var dwNameNeeded: DWORD): HResult; stdcall;
    function MicrophoneGet(pszMicrophone: PWideChar; dwMicrophoneSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function MicrophoneSet(pszMicrophone: PWideChar): HResult; stdcall;
    function SpeakerGet(pszSpeaker: PWideChar; dwSpeakerSize: DWORD;
      var dwNeeded: DWORD): HResult; stdcall;
    function SpeakerSet(pszSpeaker: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTATTRIBUTESW}
  PIVDCTATTRIBUTESW = ^IVDctAttributesW;
  {$EXTERNALSYM IVDctAttributes}
  IVDctAttributes = IVDctAttributesA;

  {$EXTERNALSYM IVDctCommandsBuiltInA}
  IVDctCommandsBuiltInA = interface(IUnknown)
    [SID_IVDctCommandsBuiltInA]
    function Activate(hWndListening: HWND; dwFlags: DWORD): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function FromMemory(pMem: Pointer; dwSize: DWORD): HResult; stdcall;
    function FromFile(szFile: PAnsiChar): HResult; stdcall;
    function FromStream(IStream: IStream): HResult; stdcall;
    function ListSet(szListName: PAnsiChar; pWordList: Pointer; dwSize: DWORD): HResult; stdcall;
    function TextGet(var pszText: PAnsiChar; var dwSize: DWORD): HResult; stdcall;
    function ToMemory(var pMem: Pointer; var dwSize: DWORD): HResult; stdcall;
    function ToFile(szFile: PAnsiChar): HResult; stdcall;
    function ToStream(IStream: IStream): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTCOMMANDSBUILTINA}
  PIVDCTCOMMANDSBUILTINA = ^IVDctCommandsBuiltInA;
  {$EXTERNALSYM IVDctCommandsBuiltInW}
  IVDctCommandsBuiltInW = interface(IUnknown)
    [SID_IVDctCommandsBuiltInW]
    function Activate(hWndListening: HWND; dwFlags: DWORD): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function FromMemory(pMem: Pointer; dwSize: DWORD): HResult; stdcall;
    function FromFile(szFile: PWideChar): HResult; stdcall;
    function FromStream(IStream: IStream): HResult; stdcall;
    function ListSet(szListName: PWideChar; pWordList: Pointer; dwSize: DWORD): HResult; stdcall;
    function TextGet(var pszText: PWideChar; var dwSize: DWORD): HResult; stdcall;
    function ToMemory(var pMem: Pointer; var dwSize: DWORD): HResult; stdcall;
    function ToFile(szFile: PWideChar): HResult; stdcall;
    function ToStream(IStream: IStream): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTCOMMANDSBUILTINW}
  PIVDCTCOMMANDSBUILTINW = ^IVDctCommandsBuiltInW;
  {$EXTERNALSYM IVDctCommandsBuiltIn}
  IVDctCommandsBuiltIn = IVDctCommandsBuiltInA;

  {$EXTERNALSYM IVDctCommandsAppA}
  IVDctCommandsAppA = interface(IUnknown)  // Except for the IID this is same as IVDctCommandsBuiltIn
    [SID_IVDctCommandsAppA]
    function Activate(hWndListening: HWND; dwFlags: DWORD): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function FromMemory(pMem: Pointer; dwSize: DWORD): HResult; stdcall;
    function FromFile(szFile: PAnsiChar): HResult; stdcall;
    function FromStream(IStream: IStream): HResult; stdcall;
    function ListSet(szListName: PAnsiChar; pWordList: Pointer; dwSize: DWORD): HResult; stdcall;
    function TextGet(var szText: PAnsiChar; var dwSize: DWORD): HResult; stdcall;
    function ToMemory(var pMem: Pointer; var dwSize: DWORD): HResult; stdcall;
    function ToFile(szFile: PAnsiChar): HResult; stdcall;
    function ToStream(IStream: IStream): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTCOMMANDSAPPA}
  PIVDCTCOMMANDSAPPA = ^IVDctCommandsAppA;
  {$EXTERNALSYM IVDctCommandsAppW}
  IVDctCommandsAppW = interface(IUnknown)  // Except for the IID this is same as IVDctCommandsBuiltIn
    [SID_IVDctCommandsAppW]
    function Activate(hWndListening: HWND; dwFlags: DWORD): HResult; stdcall;
    function Deactivate: HResult; stdcall;
    function FromMemory(pMem: Pointer; dwSize: DWORD): HResult; stdcall;
    function FromFile(szFile: PWideChar): HResult; stdcall;
    function FromStream(IStream: IStream): HResult; stdcall;
    function ListSet(szListName: PWideChar; pWordList: Pointer; dwSize: DWORD): HResult; stdcall;
    function TextGet(var szText: PWideChar; var dwSize: DWORD): HResult; stdcall;
    function ToMemory(var pMem: Pointer; var dwSize: DWORD): HResult; stdcall;
    function ToFile(szFile: PWideChar): HResult; stdcall;
    function ToStream(IStream: IStream): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTCOMMANDSAPPW}
  PIVDCTCOMMANDSAPPW = ^IVDctCommandsAppW;
  {$EXTERNALSYM IVDctCommandsApp}
  IVDctCommandsApp = IVDctCommandsAppA;


  {$EXTERNALSYM IVDctCommandsA}
  IVDctCommandsA = interface(IUnknown) // only supported in legacy discrete dictation systems
    [SID_IVDctCommandsA]
    function Add(fGlobal: BOOL; dwCmdNum: DWORD; dData: SDATA;
      var dwCmdStart: DWORD): HResult; stdcall;
    function Remove(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD): HResult; stdcall;
    function Get(fGlobal: BOOL; dwCmdStart, dwCmdNumIn, dwFlag: DWORD;
      pdData: PSDATA; var dwCmdNumOut: DWORD): HResult; stdcall;
    function DoSet(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD;
      dData: SDATA): HResult; stdcall;
    function EnableItem(fGlobal: BOOL; dwEnable, dwNum, dwFlag: DWORD): HResult; stdcall;
    function Num(fGlobal: BOOL; var dwNumCmd: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTCOMMANDSA}
  PIVDCTCOMMANDSA = ^IVDctCommandsA;
  {$EXTERNALSYM IVDctCommandsW}
  IVDctCommandsW = interface(IUnknown) // only supported in legacy discrete dictation systems
    [SID_IVDctCommandsW]
    function Add(fGlobal: BOOL; dwCmdNum: DWORD; dData: SDATA;
      var dwCmdStart: DWORD): HResult; stdcall;
    function Remove(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD): HResult; stdcall;
    function Get(fGlobal: BOOL; dwCmdStart, dwCmdNumIn, dwFlag: DWORD;
      pdData: PSDATA; var dwCmdNumOut: DWORD): HResult; stdcall;
    function DoSet(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD;
      dData: SDATA): HResult; stdcall;
    function EnableItem(fGlobal: BOOL; dwEnable, dwNum, dwFlag: DWORD): HResult; stdcall;
    function Num(fGlobal: BOOL; var dwNumCmd: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTCOMMANDSW}
  PIVDCTCOMMANDSW = ^IVDctCommandsW;
  {$EXTERNALSYM IVDctCommands}
  IVDctCommands = IVDctCommandsA;


  {$EXTERNALSYM IVDctGlossaryA}
  IVDctGlossaryA = interface(IUnknown)
    [SID_IVDctGlossaryA]
    function Add(fGlobal: BOOL; dwCmdNum: DWORD; dData: SDATA;
      var dwCmdStart: DWORD): HResult; stdcall;
    function Remove(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD): HResult; stdcall;
    function Get(fGlobal: BOOL; dwCmdStart, dwCmdNumIn, dwFlag: DWORD;
      pdData: PSDATA; var dwCmdNumOut: DWORD): HResult; stdcall;
    function DoSet(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD;
      dData: SDATA): HResult; stdcall;
    function EnableItem(fGlobal: BOOL; dwEnable, dwNum, dwFlag: DWORD): HResult; stdcall;
    function Num(fGlobal: BOOL; var dwNumCmd: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTGLOSSARYA}
  PIVDCTGLOSSARYA = ^IVDctGlossaryA;
  {$EXTERNALSYM IVDctGlossaryW}
  IVDctGlossaryW = interface(IUnknown)
    [SID_IVDctGlossaryW]
    function Add(fGlobal: BOOL; dwCmdNum: DWORD; dData: SDATA;
      var dwCmdStart: DWORD): HResult; stdcall;
    function Remove(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD): HResult; stdcall;
    function Get(fGlobal: BOOL; dwCmdStart, dwCmdNumIn, dwFlag: DWORD;
      pdData: PSDATA; var dwCmdNumOut: DWORD): HResult; stdcall;
    function DoSet(fGlobal: BOOL; dwCmdStart, dwCmdNum, dwFlag: DWORD;
      dData: SDATA): HResult; stdcall;
    function EnableItem(fGlobal: BOOL; dwEnable, dwNum, dwFlag: DWORD): HResult; stdcall;
    function Num(fGlobal: BOOL; var dwNumCmd: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTGLOSSARYW}
  PIVDCTGLOSSARYW = ^IVDctGlossaryW;
  {$EXTERNALSYM IVDctGlossary}
  IVDctGlossary = IVDctGlossaryA;


  {$EXTERNALSYM IVDctDialogsA}
  IVDctDialogsA = interface(IUnknown)
    [SID_IVDctDialogsA]
    function AboutDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainMicDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TrainGeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTDIALOGSA}
  PIVDCTDIALOGSA = ^IVDctDialogsA;
  {$EXTERNALSYM IVDctDialogsW}
  IVDctDialogsW = interface(IUnknown)
    [SID_IVDctDialogsW]
    function AboutDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainMicDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TrainGeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTDIALOGSW}
  PIVDCTDIALOGSW = ^IVDctDialogsW;
  {$EXTERNALSYM IVDctDialogs}
  IVDctDialogs = IVDctDialogsA;


  {$EXTERNALSYM IVDctGUI}
  IVDctGUI = interface(IUnknown)
    [SID_IVDctGUI]
    function SetSelRect(var Rect: TRect): HResult; stdcall;
    function FlagsSet(dwFlags: DWORD): HResult; stdcall;
    function FlagsGet(var dwFlags: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVDCTGUI}
  PIVDCTGUI = ^IVDctGUI;

//************************************************************************
// High-Level text-to-speech API
//************************************************************************

const
  // dwFlags parameter of IVoiceText::Register
  // #define  ONE                    (1)
  // #define  VTXTF_ALLMESSAGES      (ONE<<0)

  {$EXTERNALSYM VTXTF_ALLMESSAGES}
  VTXTF_ALLMESSAGES    = $00000001;

  // dwFlags parameter of IVoiceText::Speak
  // type of speech
  {$EXTERNALSYM VTXTST_STATEMENT}
  VTXTST_STATEMENT     = $00000001;
  {$EXTERNALSYM VTXTST_QUESTION}
  VTXTST_QUESTION      = $00000002;
  {$EXTERNALSYM VTXTST_COMMAND}
  VTXTST_COMMAND       = $00000004;
  {$EXTERNALSYM VTXTST_WARNING}
  VTXTST_WARNING       = $00000008;
  {$EXTERNALSYM VTXTST_READING}
  VTXTST_READING       = $00000010;
  {$EXTERNALSYM VTXTST_NUMBERS}
  VTXTST_NUMBERS       = $00000020;
  {$EXTERNALSYM VTXTST_SPREADSHEET}
  VTXTST_SPREADSHEET   = $00000040;
  // priorities
  {$EXTERNALSYM VTXTSP_VERYHIGH}
  VTXTSP_VERYHIGH      = $00000080;
  {$EXTERNALSYM VTXTSP_HIGH}
  VTXTSP_HIGH          = $00000100;
  {$EXTERNALSYM VTXTSP_NORMAL}
  VTXTSP_NORMAL        = $00000200;

type
  // possible parameter to IVoiceText::Register

  PVTSiteInfo = ^TVTSiteInfo;
  {$EXTERNALSYM VTSITEINFO}
  VTSITEINFO = record
    dwDevice: DWORD;
    dwEnable: DWORD;
    dwSpeed : DWORD;
    gModeID : TGUID;
  end;
  TVTSiteInfo = VTSITEINFO;

  {$EXTERNALSYM IVTxtNotifySinkA}
  IVTxtNotifySinkA = interface(IUnknown)
    [SID_IVTxtNotifySinkA]
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Visual(cIPAPhoneme: WideChar; cEnginePhoneme: AnsiChar; // cIPAPhoneme is WideChar even in ANSI compile
      dwHints: DWORD; pTTSMouth: PTTSMOUTH): HResult; stdcall;
    function Speak(pszText: PAnsiChar; pszApplication: PAnsiChar; dwType: DWORD): HResult; stdcall;
    function SpeakingStarted: HResult; stdcall;
    function SpeakingDone: HResult; stdcall;
  end;
  {$EXTERNALSYM PIVTXTNOTIFYSINKA}
  PIVTXTNOTIFYSINKA = ^IVTxtNotifySinkA;
  {$EXTERNALSYM IVTxtNotifySinkW}
  IVTxtNotifySinkW = interface(IUnknown)
    [SID_IVTxtNotifySinkW]
    function AttribChanged(dwAttribute: DWORD): HResult; stdcall;
    function Visual(cIPAPhoneme: WideChar; cEnginePhoneme: WideChar; // cIPAPhoneme is WideChar even in ANSI compile
      dwHints: DWORD; pTTSMouth: PTTSMOUTH): HResult; stdcall;
    function Speak(pszText: PWideChar; pszApplication: PWideChar; dwType: DWORD): HResult; stdcall;
    function SpeakingStarted: HResult; stdcall;
    function SpeakingDone: HResult; stdcall;
  end;
  {$EXTERNALSYM PIVTXTNOTIFYSINKW}
  PIVTXTNOTIFYSINKW = ^IVTxtNotifySinkW;
  {$EXTERNALSYM IVTxtNotifySink}
  IVTxtNotifySink = IVTxtNotifySinkA;

  {$EXTERNALSYM IVoiceTextA}
  IVoiceTextA = interface(IUnknown)
    [SID_IVoiceTextA]
    function Register(pszSite, pszApplication: PAnsiChar;
      NotifyInterface: IVTxtNotifySinkA; IIDNotifyInterface: TGUID;
      dwFlags: DWORD; pSiteInfo: PVTSiteInfo): HResult; stdcall;
    function Speak(pszSpeak: PAnsiChar; dwFlags: DWORD; pszTags: PAnsiChar): HResult; stdcall;
    function StopSpeaking: HResult; stdcall;
    function AudioFastForward: HResult; stdcall;
    function AudioPause: HResult; stdcall;
    function AudioResume: HResult; stdcall;
    function AudioRewind: HResult; stdcall;
  end;
  {$EXTERNALSYM PIVOICETEXTA}
  PIVOICETEXTA = ^IVoiceTextA;
  {$EXTERNALSYM IVoiceTextW}
  IVoiceTextW = interface(IUnknown)
    [SID_IVoiceTextW]
    function Register(pszSite, pszApplication: PWideChar;
      NotifyInterface: IVTxtNotifySinkW; IIDNotifyInterface: TGUID;
      dwFlags: DWORD; pSiteInfo: PVTSiteInfo): HResult; stdcall;
    function Speak(pszSpeak: PWideChar; dwFlags: DWORD; pszTags: PWideChar): HResult; stdcall;
    function StopSpeaking: HResult; stdcall;
    function AudioFastForward: HResult; stdcall;
    function AudioPause: HResult; stdcall;
    function AudioResume: HResult; stdcall;
    function AudioRewind: HResult; stdcall;
  end;
  {$EXTERNALSYM PIVOICETEXTW}
  PIVOICETEXTW = ^IVoiceTextW;
  {$EXTERNALSYM IVoiceText}
  IVoiceText = IVoiceTextA;

  {$EXTERNALSYM IVTxtAttributesA}
  IVTxtAttributesA = interface(IUnknown)
    [SID_IVTxtAttributesA]
    function DeviceGet(var dwDeviceID: DWORD): HResult; stdcall;
    function DeviceSet(dwDeviceID: DWORD): HResult; stdcall;
    function EnabledGet(var dwEnabled: DWORD): HResult; stdcall;
    function EnabledSet(dwEnabled: DWORD): HResult; stdcall;
    function IsSpeaking(var fSpeaking: BOOL): HResult; stdcall;
    function SpeedGet(var dwSpeed: DWORD): HResult; stdcall;
    function SpeedSet(dwSpeed: DWORD): HResult; stdcall;
    function TTSModeGet(var gVoice: TGUID): HResult; stdcall;
    function TTSModeSet(gVoice: TGUID): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVTXTATTRIBUTESA}
  PIVTXTATTRIBUTESA = ^IVTxtAttributesA;
  {$EXTERNALSYM IVTxtAttributesW}
  IVTxtAttributesW = interface(IUnknown)
    [SID_IVTxtAttributesW]
    function DeviceGet(var dwDeviceID: DWORD): HResult; stdcall;
    function DeviceSet(dwDeviceID: DWORD): HResult; stdcall;
    function EnabledGet(var dwEnabled: DWORD): HResult; stdcall;
    function EnabledSet(dwEnabled: DWORD): HResult; stdcall;
    function IsSpeaking(var fSpeaking: BOOL): HResult; stdcall;
    function SpeedGet(var dwSpeed: DWORD): HResult; stdcall;
    function SpeedSet(dwSpeed: DWORD): HResult; stdcall;
    function TTSModeGet(var gVoice: TGUID): HResult; stdcall;
    function TTSModeSet(gVoice: TGUID): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVTXTATTRIBUTESW}
  PIVTXTATTRIBUTESW = ^IVTxtAttributesW;
  {$EXTERNALSYM IVTxtAttributes}
  IVTxtAttributes = IVTxtAttributesA;

  {$EXTERNALSYM IVTxtDialogsA}
  IVTxtDialogsA = interface(IUnknown)
    [SID_IVTxtDialogsA]
    function AboutDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
    function TranslateDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVTXTDIALOGSA}
  PIVTXTDIALOGSA = ^IVTxtDialogsA;
  {$EXTERNALSYM IVTxtDialogsW}
  IVTxtDialogsW = interface(IUnknown)
    [SID_IVTxtDialogsW]
    function AboutDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function LexiconDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function GeneralDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
    function TranslateDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PIVTXTDIALOGSW}
  PIVTXTDIALOGSW = ^IVTxtDialogsW;
  {$EXTERNALSYM IVTxtDialogs}
  IVTxtDialogs = IVTxtDialogsA;

//**********************************************************************
// Misc Tools and parts
//**********************************************************************

  // ISTRecord Interface
  PStRecordParam = ^TStRecordParam;
  {$EXTERNALSYM STRECORDPARAM}
  STRECORDPARAM = record
    dwBetweenUtt: DWORD;  // acceptable time between utterance, milliseconds
    dwInitialBuf: DWORD;  // milliseconds of audio in the initial buffer
    dwReallocBuf: DWORD;  // amount to increase a buffer every time new audio
    dwMaxBuf: DWORD;      // maximum number of milliseconds that buffer can be
    dwNoAnswer: DWORD;    // milliseconds of silence before get no-answer
  end;
  TStRecordParam = STRECORDPARAM;

  {$EXTERNALSYM ISTRecord}
  ISTRecord = interface(IUnknown)
    [SID_ISTRecord]
  //This is completely undocumented in the SDK docs
    function Init(ud1: IUnknown; ud2: DWORD; ud3: IUnknown): HResult; stdcall;
    function ParamsSet(ud1: PSTRECORDPARAM): HResult; stdcall;
    function Start: HResult; stdcall;
    function Stop(ud1: DWORD): HResult; stdcall;
    function GetWAV(var ud1: Pointer; var ud2: DWORD): HResult; stdcall;
    function ClearWAV: HResult; stdcall;
  end;
  {$EXTERNALSYM PISTRECORD}
  PISTRECORD = ^ISTRecord;

  {$EXTERNALSYM ISTRecordNotifySink}
  ISTRecordNotifySink = interface(IUnknown)
    [SID_ISTRecordNotifySink]
  //This is completely undocumented in the SDK docs
    function AutoStop: HResult; stdcall;
    function VU(ud1: WORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTRECORDNOTIFYSINK}
  PISTRECORDNOTIFYSINK = ^ISTRecordNotifySink;

  {$EXTERNALSYM ISTGramComp}
  ISTGramComp = interface(IUnknown)      // Unicode only
    [SID_ISTGramComp]
    function FromMemory(pMem: Pointer; dwSize: DWORD): HResult; stdcall;
    function FromResource(hInst: HINST; dwNum: DWORD): HResult; stdcall;
    function FromFile(pszFile: PWideChar): HResult; stdcall;
    function FromStream(IStream: IStream): HResult; stdcall;
    function Compile(var pszError: PWideChar; pdwSize: PDWORD): HResult; stdcall;
    function IsCompiled(var fCompiled: BOOL): HResult; stdcall;
    function ToMemory(var pMem: Pointer; var dwSize: DWORD): HResult; stdcall;
    function ToFile(pszFile: PWideChar): HResult; stdcall;
    function ToStream(IStream: IStream): HResult; stdcall;
    function TextGet(var pszText: PWideChar; var dwSize: DWORD): HResult; stdcall;
    function LanguageGet(var Lang: TLanguageW): HResult; stdcall;
    function GrammarFormatGet(var Fmt: SRGRMFMT): HResult; stdcall;
    function GrammarDataGet(fEngineSpecific: BOOL;
      var pData: Pointer; var dwSize: DWORD): HResult; stdcall;
    function GrammarDataSet(pData: Pointer; dwSize: DWORD): HResult; stdcall;
    function AutoList(ISRGramCFG: ISRGramCFGW): HResult; stdcall;
    function PhraseParse(pSRPhrase: PSRPhraseW; pdwCmdID: PDWORD;
      var pszData: PWideChar; pdwSize: PDWORD): HResult; stdcall;
    function GrammarLoad(Engine: IUnknown; pNotifyInterface: pointer;
      IIDNotifyInterface: TGUID; var UnknownGrammar: IUnknown): HResult; stdcall;
    function PhraseParseAlt(dwAlt: DWORD; pdwCmdID: PDWORD;
      var pszData: PWideChar; pdwSize: PDWORD): HResult; stdcall;
    function PhraseParseRule(pSRPhrase: PSRPHRASEW; pdwCmdID: PDWORD;
      var pszData: PWideChar; pdwSize: PDWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTGRAMCOMP}
  PISTGRAMCOMP = ^ISTGramComp;

  {$EXTERNALSYM ISTTTSQueue}
  ISTTTSQueue = interface(IUnknown)
    [SID_ISTTTSQueue]
    function WaveAddFromFile(pszSpeakString: LPCWSTR; pszFile: LPCWSTR): HResult; stdcall;
    function WaveAddFromStream(pszSpeakString: LPCWSTR; IStream: IStream): HResult; stdcall;
    function WaveAddFromMemory(pszSpeakString: LPCWSTR; pMem: pointer;
      dwSize: DWORD): HResult; stdcall;
    function WaveAddFromResource(pszSpeakString: LPCWSTR; hModule: HMODULE;
      dwResID: DWORD): HResult; stdcall;
    function WaveAddFromList(pszInf: LPCWSTR; hModule: HMODULE): HResult; stdcall;
    function WaveRemove(pszSpeakString: LPCWSTR): HResult; stdcall;
    function WaveAudioDestSet(IUnk: IUnknown): HResult; stdcall;
    function WaveLevelSet(dwLevel: DWORD): HResult; stdcall;
    function WaveLevelGet(var dwLevel: DWORD): HResult; stdcall;
    function TTSAdd(pszVoice: LPCWSTR; Unk: IUnknown): HResult; stdcall;
    function TTSRemove(pszVoice: LPCWSTR): HResult; stdcall;
    function TTSGet(pszVoice: LPCWSTR; var Unk: IUnknown): HResult; stdcall;
    function Speak(pszSpeak: LPCWSTR; pszVoice: LPCWSTR; dwID: DWORD): HResult; stdcall;
    function TextData(eCharacterSet: VOICECHARSET; dwFlags: DWORD; dText: SDATA;
      pszVoice: LPCWSTR; dwID: DWORD): HResult; stdcall;
    function AudioPause: HResult; stdcall;
    function AudioResume: HResult; stdcall;
    function AudioReset: HResult; stdcall;
    function Register(pNotifyInterface: pointer; IIDNotifyInterface: TGUID): HResult; stdcall;
    function UseLogging(IUnk: IUnknown): HResult; stdcall;
    function IsSpeaking(var dwSpeak: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTTTSQUEUE}
  PISTTTSQUEUE = ^ISTTTSQueue;

  {$EXTERNALSYM ISTTTSQueueNotifySink}
  ISTTTSQueueNotifySink = interface(IUnknown)
    [SID_ISTTTSQueueNotifySink]
    function Start: HResult; stdcall;
    function Stop: HResult; stdcall;
    function SpeakID(dwID: DWORD): HResult; stdcall;
    function Error(hRes: HRESULT): HResult; stdcall;
    function Bookmark(dwMark: DWORD): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTTTSQueueNotifySink}
  PISTTTSQueueNotifySink = ^ISTTTSQueueNotifySink;

  {$EXTERNALSYM ISTLog}
  ISTLog = interface(IUnknown)
    [SID_ISTLog]
    function ToFile(pszFile: LPCWSTR): HResult; stdcall;
    function ToStream(IStream: IStream): HResult; stdcall;
    function Text(pszSource: LPCWSTR; pszMessage: LPCWSTR; wDetail: WORD): HResult; stdcall;
    function Data(pszSource: LPCWSTR; pszMessage: LPCWSTR; wDetail: WORD;
      pData: pointer; dwSize: DWORD): HResult; stdcall;
    function MaxDetailSet(wMaxDetail: WORD): HResult; stdcall;
    function MaxDetailGet(var wMaxDetail: WORD): HResult; stdcall;
    function ShowOutput(fShow: BOOL): HResult; stdcall;
  end;
  {$EXTERNALSYM PISTLOG}
  PISTLOG = ^ISTLog;

  {$EXTERNALSYM IAudioSourceLog}
  IAudioSourceLog = interface(IUnknown)
    [SID_IAudioSourceLog]
    function AudioSource(AudioSource: IUnknown): HResult; stdcall;
    function UseLogging(IUnk: IUnknown): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOSOURCELOG}
  PIAUDIOSOURCELOG = ^IAudioSourceLog;

  {$EXTERNALSYM IAudioTel}
  IAudioTel = interface(IUnknown)
    function AudioObject(AudioObject: IUnknown): HResult; stdcall;
    function WaveFormatSet(dWFEX: SDATA): HResult; stdcall;
  end;
  {$EXTERNALSYM PIAUDIOTEL}
  PIAUDIOTEL = ^IAudioTel;

//************************************************************************
// External speech synthesizer - This is for using external, hardware
// based speech synthesizers with SAPI
//************************************************************************
const
// Synthesizer Attributes
  {$EXTERNALSYM TTS_LANGUAGE}
  TTS_LANGUAGE                     = $00000001 ;     //{bit 0}
  {$EXTERNALSYM TTS_VOICE}
  TTS_VOICE                        = $00000002 ;     //{bit 1}
  {$EXTERNALSYM TTS_GENDER}
  TTS_GENDER                       = $00000004 ;     //{bit 2}
  {$EXTERNALSYM TTS_VOLUME}
  TTS_VOLUME                       = $00000008 ;     //{bit 3}
  {$EXTERNALSYM TTS_PITCH}
  TTS_PITCH                        = $00000010 ;     //{bit 4}
  {$EXTERNALSYM TTS_SPEED}
  TTS_SPEED                        = $00000020 ;     //{bit 5}
  {$EXTERNALSYM TTS_ABBREVIATION}
  TTS_ABBREVIATION                 = $00000040 ;     //{bit 6}
  {$EXTERNALSYM TTS_PUNCTUATION}
  TTS_PUNCTUATION                  = $00000080 ;     //{bit 7}
  {$EXTERNALSYM TTS_PAUSEWORD}
  TTS_PAUSEWORD                    = $00000100 ;     //{bit 8}
  {$EXTERNALSYM TTS_PAUSEPHRASE}
  TTS_PAUSEPHRASE                  = $00000200 ;     //{bit 9}
  {$EXTERNALSYM TTS_PAUSESENTENCE}
  TTS_PAUSESENTENCE                = $00000400 ;     //{bit 10}
  {$EXTERNALSYM TTS_SPELLING}
  TTS_SPELLING                     = $00000800 ;     //{bit 11}
  {$EXTERNALSYM TTS_QUALITY}
  TTS_QUALITY                      = $00001000 ;     //{bit 12}
  {$EXTERNALSYM TTS_FRICATION}
  TTS_FRICATION                    = $00002000 ;     //{bit 13}
  {$EXTERNALSYM TTS_ASPIRATION}
  TTS_ASPIRATION                   = $00004000 ;     //{bit 14}
  {$EXTERNALSYM TTS_INTONATION}
  TTS_INTONATION                   = $00008000 ;     //{bit 15}

// Attribute minimums and maximums
  {$EXTERNALSYM TTSATTR_MINPAUSEWORD}
  TTSATTR_MINPAUSEWORD     = $00000000;
  {$EXTERNALSYM TTSATTR_MAXPAUSEWORD}
  TTSATTR_MAXPAUSEWORD     = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MINPAUSEPHRASE}
  TTSATTR_MINPAUSEPHRASE   = $00000000;
  {$EXTERNALSYM TTSATTR_MAXPAUSEPHRASE}
  TTSATTR_MAXPAUSEPHRASE   = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MINPAUSESENTENCE}
  TTSATTR_MINPAUSESENTENCE = $00000000;
  {$EXTERNALSYM TTSATTR_MAXPAUSESENTENCE}
  TTSATTR_MAXPAUSESENTENCE = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MINASPIRATION}
  TTSATTR_MINASPIRATION    = $00000000;
  {$EXTERNALSYM TTSATTR_MAXASPIRATION}
  TTSATTR_MAXASPIRATION    = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MINFRICATION}
  TTSATTR_MINFRICATION     = $00000000;
  {$EXTERNALSYM TTSATTR_MAXFRICATION}
  TTSATTR_MAXFRICATION     = DWORD($ffffffff);
  {$EXTERNALSYM TTSATTR_MININTONATION}
  TTSATTR_MININTONATION    = $00000000;
  {$EXTERNALSYM TTSATTR_MAXINTONATION}
  TTSATTR_MAXINTONATION    = DWORD($ffffffff);

// ITTSNotifySink
  {$EXTERNALSYM TTSNSAC_LANGUAGE}
  TTSNSAC_LANGUAGE       =  100;
  {$EXTERNALSYM TTSNSAC_VOICE}
  TTSNSAC_VOICE          =  101;
  {$EXTERNALSYM TTSNSAC_GENDER}
  TTSNSAC_GENDER         =  102;
  {$EXTERNALSYM TTSNSAC_ABBREVIATION}
  TTSNSAC_ABBREVIATION   =  103;
  {$EXTERNALSYM TTSNSAC_PUNCTUATION}
  TTSNSAC_PUNCTUATION    =  104;
  {$EXTERNALSYM TTSNSAC_PAUSEWORD}
  TTSNSAC_PAUSEWORD      =  105;
  {$EXTERNALSYM TTSNSAC_PAUSEPHRASE}
  TTSNSAC_PAUSEPHRASE    =  106;
  {$EXTERNALSYM TTSNSAC_PAUSESENTENCE}
  TTSNSAC_PAUSESENTENCE  =  107;
  {$EXTERNALSYM TTSNSAC_SPELLING}
  TTSNSAC_SPELLING       =  108;
  {$EXTERNALSYM TTSNSAC_QUALITY}
  TTSNSAC_QUALITY        =  109;
  {$EXTERNALSYM TTSNSAC_FRICATION}
  TTSNSAC_FRICATION      =  110;
  {$EXTERNALSYM TTSNSAC_ASPIRATION}
  TTSNSAC_ASPIRATION     =  111;
  {$EXTERNALSYM TTSNSAC_INTONATION}
  TTSNSAC_INTONATION     =  112;

  // Interfaces
  {$EXTERNALSYM TTSI_ITTSEXTERNALSYNTHESIZER}
  TTSI_ITTSEXTERNALSYNTHESIZER = $00000010 ;     //{bit 4}   SETBIT(4)

  // TTSERR Result codes
  {$EXTERNALSYM TTSERR_SYNTHESIZERBUSY}
  TTSERR_SYNTHESIZERBUSY              = HRESULT($80040564);
  {$EXTERNALSYM TTSERR_ALREADYDISPLAYED}
  TTSERR_ALREADYDISPLAYED             = HRESULT($80040565);
  {$EXTERNALSYM TTSERR_INVALIDATTRIB}
  TTSERR_INVALIDATTRIB                = HRESULT($80040566);
  {$EXTERNALSYM TTSERR_SYNTHESIZERACCESSERROR}
  TTSERR_SYNTHESIZERACCESSERROR       = HRESULT($80040567);
  {$EXTERNALSYM TTSERR_DRIVERERROR}
  TTSERR_DRIVERERROR                  = HRESULT($80040568);
  {$EXTERNALSYM TTSERR_UNRECOVERABLEERROR}
  TTSERR_UNRECOVERABLEERROR           = HRESULT($80040569);
  {$EXTERNALSYM TTSERR_DRIVERACCESSERROR}
  TTSERR_DRIVERACCESSERROR            = HRESULT($8004056A);
  {$EXTERNALSYM TTSERR_BUFFERTOOSMALL}
  TTSERR_BUFFERTOOSMALL               = HRESULT($8004056B);
  {$EXTERNALSYM TTSERR_DRIVERNOTFOUND}
  TTSERR_DRIVERNOTFOUND               = HRESULT($8004056C);
  {$EXTERNALSYM TTSERR_CANNOTREGISTER}
  TTSERR_CANNOTREGISTER               = HRESULT($8004056D);
  {$EXTERNALSYM TTSERR_LANGUAGENOTSUPPORTED}
  TTSERR_LANGUAGENOTSUPPORTED         = HRESULT($8004056E);

type

  PTTSPreset = ^TTTSPreset;
  {$EXTERNALSYM TTSPRESET}
  TTSPRESET  = record
    Lang:           LANGID;
    Voice:          DWORD;
    Gender:         DWORD;
    Volume:         DWORD;
    Pitch:          DWORD;
    Speed:          DWORD;
    Abbreviation:   DWORD;
    Punctuation:    DWORD;
    PauseWord:      DWORD;
    PausePhrase:    DWORD;
    PauseSentence:  DWORD;
    Spelling:       DWORD;
    Quality:        DWORD;
    Frication:      DWORD;
    Aspiration:     DWORD;
    Intonation:     DWORD;
  end;
  TTTSPreset = TTSPRESET;

  {$EXTERNALSYM ITTSExternalSynthesizerA}
  ITTSExternalSynthesizerA = interface(IUnknown)
    [SID_ITTSExternalSynthesizerA]
    function AttributeGet(dwAttribute: DWORD; var dwValue: DWORD): HResult; stdcall;
    function AttributeSet(dwAttribute: DWORD; dwValue: DWORD): HResult; stdcall;
    function PresetGet(pPreset: PTTSPRESET; var dwResult: DWORD): HResult; stdcall;
    function PresetSet(Preset: TTSPRESET; var dwResult: DWORD): HResult; stdcall;
    function GetAttributeElements(dwAttr: DWORD; var dwNumElems: DWORD): HResult; stdcall;
    function GetAttributeRange(dwAttr: DWORD; var dwMin: DWORD;
      var dwMax: DWORD): HResult; stdcall;
    function GetAttributeValue(dwAttr: DWORD; dwIndex: DWORD; var dwValue: DWORD;
      szText: PAnsiChar; dwMaxChars: DWORD): HResult; stdcall;
    function GetAttributeIndex(dwAttr: DWORD; dwValue: DWORD; var dwIndex: DWORD;
      szText: PAnsiChar; dwMaxChars: DWORD): HResult; stdcall;
    function AudioFlush: HResult; stdcall;
    function TextDataInterrupt(eCharacterSet: VOICECHARSET; dwFlags: DWORD;
      dText: SDATA; pNotifyInterface: pointer; IIDNotifyInterface: TGUID): HResult; stdcall;
    function TextDataCritical(SomeData: SDATA): HResult; stdcall;
    function MiscellaneousDlg(hWndParent: HWND; pszTitle: PAnsiChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSEXTERNALSYNTHESIZERA}
  PITTSEXTERNALSYNTHESIZERA = ^ITTSExternalSynthesizerA;
  {$EXTERNALSYM ITTSExternalSynthesizerW}
  ITTSExternalSynthesizerW = interface(IUnknown)
    [SID_ITTSExternalSynthesizerW]
    function AttributeGet(dwAttribute: DWORD; var dwValue: DWORD): HResult; stdcall;
    function AttributeSet(dwAttribute: DWORD; dwValue: DWORD): HResult; stdcall;
    function PresetGet(pPreset: PTTSPRESET; var dwResult: DWORD): HResult; stdcall;
    function PresetSet(Preset: TTSPRESET; var dwResult: DWORD): HResult; stdcall;
    function GetAttributeElements(dwAttr: DWORD; var dwNumElems: DWORD): HResult; stdcall;
    function GetAttributeRange(dwAttr: DWORD; var dwMin: DWORD;
      var dwMax: DWORD): HResult; stdcall;
    function GetAttributeValue(dwAttr: DWORD; dwIndex: DWORD; var dwValue: DWORD;
      szText: PWideChar; dwMaxChars: DWORD): HResult; stdcall;
    function GetAttributeIndex(dwAttr: DWORD; dwValue: DWORD; var dwIndex: DWORD;
      szText: PWideChar; dwMaxChars: DWORD): HResult; stdcall;
    function AudioFlush: HResult; stdcall;
    function TextDataInterrupt(eCharacterSet: VOICECHARSET; dwFlags: DWORD;
      dText: SDATA; pNotifyInterface: pointer; IIDNotifyInterface: TGUID): HResult; stdcall;
    function TextDataCritical(SomeData: SDATA): HResult; stdcall;
    function MiscellaneousDlg(hWndParent: HWND; pszTitle: PWideChar): HResult; stdcall;
  end;
  {$EXTERNALSYM PITTSEXTERNALSYNTHESIZERW}
  PITTSEXTERNALSYNTHESIZERW = ^ITTSExternalSynthesizerW;
  {$EXTERNALSYM ITTSExternalSynthesizer}
  ITTSExternalSynthesizer = ITTSExternalSynthesizerA;

  //And now for something truely useless, undocumented with no methods besides the big 3
  {$EXTERNALSYM IAudioExternalSynthesizer}
  IAudioExternalSynthesizer = interface(IUnknown)
    [SID_IAudioExternalSynthesizer]
  end;
  {$EXTERNALSYM PIAUDIOEXTERNALSYNTHESIZER}
  PIAUDIOEXTERNALSYNTHESIZER = ^IAudioExternalSynthesizer;

implementation

end. // Thats all folks!
